# This is an example validation module.
# The function name must match the name specified in your JSON filter and config.json.
# It must accept 'path' and 'params' (the parsed JSON) as arguments.
# It must return a list with is_valid = TRUE/FALSE and a message.

source("modules/utils.R")

perform_ards_check <- function(path, params) {
  
  # --- 1. File and Parameter Validation ---
  if (is.null(params$filter)) {
    return(list(is_valid = FALSE, message = "JSON is missing the required 'filter' key."))
  }
  
  if (!file.exists(path)) {
    return(list(is_valid = FALSE, message = paste("Data file not found at:", path)))
  }
  
  # --- 2. Data Loading ---
  ards_data <- tryCatch({
    ext <- tools::file_ext(path)
    if (ext == "sas7bdat") haven::read_sas(path)
    else if (ext == "csv") read.csv(path, stringsAsFactors = FALSE)
    else stop(paste("Unsupported file type:", ext))
  }, error = function(e) {
    return(list(is_valid = FALSE, message = paste("Error reading data file:", e$message)))
  })
  if (!is.data.frame(ards_data)) return(ards_data)
  
  # --- 3. Business Logic ---
  
  # Extract params from JSON, providing defaults
  filter_query         <- params$filter %||% "TRUE"
  measure              <- params$measure %||% NA            # result type literal used to show treatment value
  difference_measure   <- params$difference_measure %||% NA # result type literal used to show treatment-comparator effect
  difference_lci       <- params$difference_lci %||% NA     # result type literal used to show treatment-comparator effect lower CI
  difference_uci       <- params$difference_uci %||% NA     # result type literal used to show treatment-comparator effect upper CI
  cmp_name             <- params$cmp_name %||% "Placebo"    # comparator name in reference treatment column
  ref_column           <- tolower(params$ref_column) %||% "reftrt" # name of the reference column
  trt_column           <- tolower(params$trt_column) %||% "trt"    # name of the treatment column
  resulttype_column    <- tolower(params$resulttype_column) %||% "resulttype" # name of the result type column
  result_column        <- tolower(params$result_column) %||% "result"         # name of the result column
  
  df_result <- tryCatch({
    ards_data <- ards_data %>% rename_all(tolower)
    
    filter_query <- lowercase_query_vars(filter_query)
    
    if (!measure %in% unique(ards_data[[resulttype_column]][ards_data[[ref_column]]==""])) {
      stop(sprintf("Measure %s not in [%s]\n", 
                   measure, 
                   paste(unique(ards_data[[resulttype_column]][ards_data[[ref_column]]==""]), collapse=', ')))
    }
    
    if (!all(c(difference_measure, difference_lci, difference_uci) %in% unique(ards_data[[resulttype_column]][ards_data[[ref_column]]!=""]))) {
      stop(sprintf("Difference Measure (or CIs) not in [%s]\n",
                   paste(unique(ards_data[[resulttype_column]][ards_data[[ref_column]]!=""]), collaspe=', ')))
    }
    
    filter_expr   <- rlang::parse_expr(filter_query)
    general_query <- gsub(paste0(ref_column, "\\s*==\\s*['\"].+?['\"]"), 'TRUE', filter_query)
    general_expr  <- rlang::parse_expr(sprintf("%s & %s==''", general_query, ref_column))
    
    p1 <-
      ards_data %>%
      filter(!!general_expr) %>%
      select_at(unique(c(trt_column, resulttype_column, result_column))) %>%
      filter(.data[[resulttype_column]] %in% c(measure)) %>%
      pivot_wider(id_cols=!!trt_column, names_from=!!resulttype_column, values_from=!!result_column) %>%
      select_at(c(trt_column, measure)) %>%
      rename(
        trt_value := !!measure
      ) %>%
      mutate(
        cmp_value = .data[[measure]][.data[[trt_column]]==cmp_name]
      ) 
    
    ards_data %>%
      filter(!!filter_expr) %>%
      filter(.data[[ref_column]]==!!cmp_name) %>%
      select_at(unique(c(trt_column, resulttype_column, result_column))) %>%
      filter(.data[[resulttype_column]] %in% c(difference_measure, difference_lci, difference_uci)) %>%
      pivot_wider(id_cols=!!trt_column, names_from=!!resulttype_column, values_from=!!result_column) %>%
      select_at(c(trt_column, difference_measure, difference_lci, difference_uci)) %>%
      rename(
        effect_estimate := !!difference_measure,
        effect_lower_ci := !!difference_lci,
        effect_upper_ci := !!difference_uci
      ) %>%
      left_join(p1) %>%
      rename(trt_name := !!trt_column) %>%
      mutate(cmp_name=!!cmp_name) %>%
      select(cmp_name, cmp_value, trt_name, trt_value, effect_estimate, effect_lower_ci, effect_upper_ci) %>%
      filter(trt_name!=!!cmp_name)
    
  }, error = function(e) {
    return(list(is_valid = FALSE, message = paste("Error during data extraction/filtering:", e$message)))
  })
  
  if (!is.data.frame(df_result)) return(df_result)
  
  # --- 4. Return Success ---
  # On success, is_valid is TRUE and the message is the resulting dataframe.
  # The main app will format this dataframe for the tooltip.
  return(list(is_valid = TRUE, message = df_result))
}
