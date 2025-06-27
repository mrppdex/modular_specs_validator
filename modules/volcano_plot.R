# This module creates a volcano plot from a given dataset.
# The function name must match the name specified in your JSON filter and config.json.
# It must accept 'path' and 'params' (the parsed JSON) as arguments.
# It must return a list with is_valid = TRUE/FALSE and a message.
# The message on success will be a plotly object.

volcano_plot <- function(path, params) {

  # --- 1. Parameter Validation ---
  required_params <- c("filter", "pt_column", "trt_column", "result_column", "resulttype_column", "pval_name", "effect_name")
  missing_params <- setdiff(required_params, names(params))
  if (length(missing_params) > 0) {
    return(list(is_valid = FALSE, message = paste("JSON is missing required keys:", paste(missing_params, collapse = ", "))))
  }

  if (!file.exists(path)) {
    return(list(is_valid = FALSE, message = paste("Data file not found at:", path)))
  }

  # --- 2. Data Loading ---
  data <- tryCatch({
    ext <- tools::file_ext(path)
    if (ext == "sas7bdat") haven::read_sas(path)
    else if (ext == "csv") read.csv(path, stringsAsFactors = FALSE)
    else stop(paste("Unsupported file type:", ext))
  }, error = function(e) {
    return(list(is_valid = FALSE, message = paste("Error reading data file:", e$message)))
  })
  if (!is.data.frame(data)) return(data)

  # --- 3. Data Processing ---
  df_result <- tryCatch({
    data <- data %>% rename_all(tolower)

    # Extract params from JSON
    filter_query      <- params$filter
    pt_column         <- tolower(params$pt_column)
    trt_column        <- tolower(params$trt_column)
    result_column     <- tolower(params$result_column)
    resulttype_column <- tolower(params$resulttype_column)
    pval_name         <- params$pval_name
    effect_name       <- params$effect_name

    # Check if columns exist
    required_cols <- c(pt_column, trt_column, result_column, resulttype_column)
    missing_cols <- setdiff(required_cols, colnames(data))
    if (length(missing_cols) > 0) {
      stop(paste("The following columns are missing from the data:", paste(missing_cols, collapse = ", ")))
    }

    # in the filter query replace all variable with their lowercase versions
    filter_query_masked <- str_replace_all(filter_query, "'[^']*'|\"[^\"]*\"", "__STRING__")
    regex_pattern       <- '\\b[[:alnum:]_\\.]+(?=\\s*(==|!=|<=|>=|<|>|%IN%|%in%|%In%))'
    vars <- str_extract_all(filter_query_masked, regex_pattern)[[1]]
    
    for (v in unique(vars)) {
      filter_query <- str_replace_all(filter_query,
                                      paste0('\\b', v, '\\b(?=\\s*(==|!=|<=|>=|<|>|%IN%|%in%|%In%))'),
                                      tolower(v))
    }
    
    # Apply filter
    filter_expr <- rlang::parse_expr(filter_query)
    filtered_data <- data %>% filter(!!filter_expr)

    # Pivot data to get effect and p-value in columns
    plot_data <- filtered_data %>% 
      filter(.data[[resulttype_column]] %in% c(effect_name, pval_name)) %>%
      pivot_wider(
        id_cols = c(!!pt_column, !!trt_column),
        names_from = !!resulttype_column,
        values_from = !!result_column
      ) %>%
      rename(
        effect = !!effect_name,
        pval = !!pval_name
      )

    # Convert to numeric and calculate -log10(p)
    plot_data$effect <- as.numeric(plot_data$effect)
    plot_data$pval <- as.numeric(plot_data$pval)
    plot_data$log_pval <- -log10(plot_data$pval)

    # Remove rows with NA/Inf values
    plot_data <- plot_data %>% 
      filter(is.finite(effect) & is.finite(log_pval))
      
    plot_data

  }, error = function(e) {
    return(list(is_valid = FALSE, message = paste("Error during data processing:", e$message)))
  })

  if (!is.data.frame(df_result)) return(df_result)
  
  if (nrow(df_result) == 0) {
      return(list(is_valid = FALSE, message = "No data available to plot after filtering."))
  }


  # --- 4. Create Volcano Plot ---
  p <- plot_ly(
    data = df_result,
    x = ~effect,
    y = ~log_pval,
    text = ~get(params$pt_column), # Use original case for hover text
    hoverinfo = 'text',
    type = 'scatter',
    mode = 'markers',
    marker = list(size = 5, opacity = 0.5)
  ) %>%
  layout(
    title = "Volcano Plot",
    xaxis = list(title = "Effect Size"),
    yaxis = list(title = "-log10(p-value)")
  )


  # --- 5. Return Success ---
  return(list(is_valid = TRUE, message = p))
}
