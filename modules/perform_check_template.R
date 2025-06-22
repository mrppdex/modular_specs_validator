# This is a template for creating a new custom validation module.
#
# INSTRUCTIONS:
# 1. Copy this file to a new file in the 'modules' directory (e.g., 'my_new_check.R').
# 2. Rename the function from 'perform_check_template' to a unique name (e.g., 'my_new_check').
# 3. Add the new function name and file path to your 'config.json' file.
# 4. Implement your custom business logic in the "Business Logic" section below.
#
# REQUIRED LIBRARIES (ensure they are loaded in app.R if needed):
# library(dplyr)
# library(rlang)
# library(haven)

perform_check_template <- function(path, params) {
  
  # --- 1. File and Parameter Validation ---
  # Check for required keys in the JSON parameters.
  if (is.null(params$filter)) {
    return(list(is_valid = FALSE, message = "JSON is missing the required 'filter' key."))
  }
  
  # Check if the data file exists.
  if (!file.exists(path)) {
    return(list(is_valid = FALSE, message = paste("Data file not found at:", path)))
  }
  
  # --- 2. Data Loading ---
  # Safely load the dataset.
  data <- tryCatch({
    ext <- tools::file_ext(path)
    if (ext == "sas7bdat") haven::read_sas(path)
    else if (ext == "csv") read.csv(path, stringsAsFactors = FALSE)
    else stop(paste("Unsupported file type:", ext))
  }, error = function(e) {
    return(list(is_valid = FALSE, message = paste("Error reading data file:", e$message)))
  })
  if (!is.data.frame(data)) return(data)
  
  # --- 3. Business Logic ---
  # This is where you will implement your custom validation and data extraction.
  # You have access to the full dataframe `data` and all JSON parameters `params`.
  
  # Use a tryCatch block to handle any potential errors during your logic.
  df_result <- tryCatch({
    
    # Example: A simple filter operation.
    # Replace this with your own complex logic.
    data %>%
      dplyr::filter(!!rlang::parse_expr(params$filter))
    
  }, error = function(e) {
    # If your logic fails, return a clear error message.
    return(list(is_valid = FALSE, message = paste("Error during custom validation:", e$message)))
  })
  
  # If the tryCatch block caught an error, df_result will be the error list. Return it.
  if (!is.data.frame(df_result)) return(df_result)
  
  # Check for zero rows after filtering, if this constitutes a failure.
  if (nrow(df_result) == 0) {
    return(list(is_valid = FALSE, message = "Validation failed: Filter condition resulted in zero rows."))
  }
  
  # --- 4. Return Success ---
  # On success, is_valid must be TRUE and the message should be the resulting dataframe.
  # The main app will format this dataframe for display in the tooltip.
  return(list(is_valid = TRUE, message = df_result))
}
