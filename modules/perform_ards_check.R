# This module performs a complex check on an ARDS dataset.
#
# REQUIRED LIBRARIES (ensure they are loaded in app.R):
# library(dplyr)
# library(rlang)
# library(haven)
# library(stringr)
# library(tidyr)

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
  df_result <- tryCatch({
    
    # --- 3.1. Extract and Prepare Parameters ---
    ards_data_lower <- ards_data %>% dplyr::rename_with(tolower)
    
    # Extract parameters from JSON, providing defaults
    filter_query       <- params$filter
    measure            <- params$measure %||% NA
    difference_measure <- params$difference_measure %||% NA
    difference_lci     <- params$difference_lci %||% NA
    difference_uci     <- params$difference_uci %||% NA
    cmp_name           <- params$cmp_name %||% "Placebo"
    ref_column         <- tolower(params$ref_column %||% "reftrt")
    trt_column         <- tolower(params$trt_column %||% "trt")
    resulttype_column  <- tolower(params$resulttype_column %||% "resulttype")
    result_column      <- tolower(params$result_column %||% "result")
    
    # --- 3.2. Check for Column Existence ---
    required_cols <- c(ref_column, trt_column, resulttype_column, result_column)
    missing_cols <- setdiff(required_cols, names(ards_data_lower))
    if (length(missing_cols) > 0) {
      stop(paste("The following required columns are missing from the dataset:", paste(missing_cols, collapse = ", ")))
    }
    
    # --- 3.3. Check for Measure/Difference Existence ---
    if (!is.na(measure) && !measure %in% unique(ards_data_lower[[resulttype_column]][ards_data_lower[[ref_column]] == ""])) {
      stop(sprintf("Measure '%s' not found for non-reference treatments.", measure))
    }
    
    required_diff_measures <- c(difference_measure, difference_lci, difference_uci)
    if (!all(is.na(required_diff_measures))) {
      available_diff_measures <- unique(ards_data_lower[[resulttype_column]][ards_data_lower[[ref_column]] != ""])
      missing_diffs <- setdiff(required_diff_measures, available_diff_measures)
      if(length(missing_diffs) > 0) {
        stop(sprintf("Difference Measure(s) '%s' not found for reference treatments.", paste(missing_diffs, collapse=", ")))
      }
    }
    
    # --- 3.4. Prepare and Execute Queries ---
    # To handle case-insensitivity, convert variables in filter to lowercase
    # NOTE: This assumes variable names in the filter query match the case in the raw dataset.
    for (v in unique(names(ards_data))) {
      filter_query <- stringr::str_replace_all(filter_query, paste0('\\b', v, '\\b'), tolower(v))
    }
    
    filter_expr <- rlang::parse_expr(filter_query)
    
    # Query for treatment effect (p1)
    p1_data <- ards_data_lower %>%
      dplyr::filter(.data[[resulttype_column]] == measure) %>%
      dplyr::select(all_of(c(trt_column, result_column))) %>%
      dplyr::rename(trt_value = all_of(result_column))
    
    cmp_value_df <- p1_data %>% 
      filter(.data[[trt_column]] == cmp_name) %>%
      select(cmp_value = trt_value)
    
    if(nrow(cmp_value_df) == 0) stop(paste("Comparator name '", cmp_name, "' not found in treatment column '", trt_column, "'."))
    
    cmp_value_scalar <- cmp_value_df$cmp_value[1]
    
    p1 <- p1_data %>%
      mutate(cmp_value = cmp_value_scalar, cmp_name = cmp_name)
    
    # Main query for difference effect
    ards_data_lower %>%
      dplyr::filter(!!filter_expr) %>%
      dplyr::select(all_of(unique(c(trt_column, ref_column, resulttype_column, result_column)))) %>%
      tidyr::pivot_wider(id_cols = all_of(c(trt_column, ref_column)), 
                         names_from = all_of(resulttype_column), 
                         values_from = all_of(result_column)) %>%
      dplyr::rename(
        effect_estimate = all_of(difference_measure),
        effect_lower_ci = all_of(difference_lci),
        effect_upper_ci = all_of(difference_uci)
      ) %>%
      dplyr::left_join(p1, by = trt_column) %>%
      dplyr::select(cmp_name, cmp_value, trt_name = trt_column, trt_value, effect_estimate, effect_lower_ci, effect_upper_ci) %>%
      dplyr::filter(trt_name != cmp_name)
    
  }, error = function(e) {
    return(list(is_valid = FALSE, message = paste("Error during data extraction/filtering:", e$message)))
  })
  
  if (!is.data.frame(df_result)) return(df_result)
  if (nrow(df_result) == 0) return(list(is_valid = FALSE, message = "Final query resulted in zero rows."))
  
  # --- 4. Return Success ---
  return(list(is_valid = TRUE, message = df_result))
}
