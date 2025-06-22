# This module contains the standard, single-column validation functions.

validate_value <- function(value, rule) {
  if (rule$Type == "Categorical") {
    if (is.na(value) || value == "") {
      return(list(is_valid = FALSE, message = "Value is required and cannot be empty."))
    }
  } else {
    if (is.na(value) || value == "") {
      return(list(is_valid = TRUE, message = NA_character_))
    }
  }
  
  value_coerced <- tryCatch({
    switch(rule$Type,
           "Numeric" = as.numeric(value),
           "Categorical" = as.character(value),
           "File Path" = as.character(value),
           "Text" = as.character(value),
           "Regex" = as.character(value) # Regex works on characters
    )
  }, warning = function(w) { NULL }, error = function(e) { NULL })
  
  is_valid <- switch(
    rule$Type,
    "Text" = is.character(value_coerced),
    "Numeric" = !is.na(value_coerced) && is.numeric(value_coerced),
    "Categorical" = {
      params <- strsplit(rule$Values, ";")[[1]]
      allowed_values_str <- sub("Values=", "", params[grepl("Values=", params)])
      allowed_values <- strsplit(allowed_values_str, ",")[[1]] %>% trimws()
      value_coerced %in% allowed_values
    },
    "File Path" = {
      path_val <- trimws(value_coerced)
      is.character(path_val) && (file.exists(path_val) || dir.exists(path_val))
    },
    # NEW: Regex validation logic
    "Regex" = {
      params <- strsplit(rule$Values, ";")[[1]]
      pattern_str <- sub("Pattern=", "", params[grepl("Pattern=", params)])
      grepl(pattern_str, value_coerced)
    },
    FALSE 
  )
  
  # Create the error message if invalid
  message <- if (!is_valid) {
    if (rule$Type == "Categorical") {
      params <- strsplit(rule$Values, ";")[[1]]
      allowed_values_str <- sub("Values=", "", params[grepl("Values=", params)])
      paste0("Value must be one of: ", allowed_values_str)
    } else if (rule$Type == "Regex") {
      params <- strsplit(rule$Values, ";")[[1]]
      pattern_str <- sub("Pattern=", "", params[grepl("Pattern=", params)])
      paste0("Value does not match the required pattern: ", pattern_str)
    } else {
      switch(
        rule$Type,
        "Text" = "Invalid text format.",
        "Numeric" = "Value must be a number.",
        "File Path" = "File or directory path does not exist."
      )
    }
  } else {
    NA_character_
  }
  
  return(list(is_valid = is_valid, message = message))
}
