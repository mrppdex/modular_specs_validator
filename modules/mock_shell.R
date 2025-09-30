# This module creates a volcano plot from a given dataset.
# The function name must match the name specified in your JSON filter and config.json.
# It must accept 'path' and 'params' (the parsed JSON) as arguments.
# It must return a list with is_valid = TRUE/FALSE and a message.
# The message on success will be a plotly object.

source("modules/utils.R")

mock_shell <- function(path, params) {
  
  if (!file.exists(path)) {
    return(list(is_valid = FALSE, message = paste("Data file not found at:", path)))
  }
  
  # --- 2. Data Loading ---
  data <- tryCatch({
    ext <- tools::file_ext(path)
    if (ext == "sas7bdat") haven::read_sas(path)
    else if (ext == "rds") readRDS(path)
    else if (ext == "csv") read.csv(path, stringsAsFactors = FALSE)
    else stop(paste("Unsupported file type:", ext))
  }, error = function(e) {
    return(list(is_valid = FALSE, message = paste("Error reading data file:", e$message)))
  })
  if (!is.data.frame(data)) return(data)
  
  # --- 3. Data Processing ---
  df_result <- tryCatch({
    data <- data %>% rename_all(tolower)
    
    pivot_ards <- function(ards) {
      print(ards %>% rename_all(tolower) %>% filter(resulttype=='N') %>% select(trt, result))
      ards %>% 
        rename_all(tolower) %>% 
        select('trt', all_of(c(ends_with('label'), starts_with(('result'))))) %>%
        mutate(indentation=4-rowSums(is.na(.))) %>%
        mutate(label=coalesce(cat4label, cat3label, cat2label, cat1label)) %>%
        filter(resulttype!='N') %>%
        select(trt, label, indentation, resulttype, result) %>%
        group_by(trt, label, indentation) %>%
        mutate(resulttypen=row_number()) %>%
        ungroup() %>% 
        pivot_wider(
          id_cols=c(trt, label, indentation),
          names_from=c(resulttype, resulttypen),
          values_from=result
        ) %>%
        mutate(label=paste0(strrep(' ', indentation*3), label)) %>% 
        mutate(across(starts_with('Percentage'), ~ round(., 1))) %>%
        select(-indentation)
    }
    
    pivoted_data <- pivot_ards(data)
    
    value_columns <-  setdiff(colnames(pivoted_data), c('trt', 'label'))
    
    percentage_open_vec <- c("(", "(", "[", "<", "{")
    percentage_close_vec <- c(")", ")", "]", ">", "}")
    
    rtf_data <-
      pivoted_data %>% 
      mutate(
        across(
          starts_with('Percentage'), ~ ifelse(
            !is.na(.), 
            sprintf("%s%.2f%s", 
                    percentage_open_vec[as.numeric(str_extract(cur_column(), "\\d+"))],
                    .x,
                    percentage_close_vec[as.numeric(str_extract(cur_column(), "\\d+"))]),
            as.character(.x)
          )
        )
      ) %>%
      mutate_all(as.character) %>%
      mutate(across(everything(), ~ ifelse(is.na(.), "", .))) %>%
      rowwise() %>%
      mutate(
        value = str_c(c_across(all_of(value_columns)), collapse = " ")
      ) %>%
      ungroup() %>% 
      pivot_wider(id_cols=c(label), names_from=c(trt), values_from=value)
    
    rtf_data
    
  }, error = function(e) {
    return(list(is_valid = FALSE, message = paste("Error during data processing:", e$message)))
  })
  
  if (!is.data.frame(df_result)) return(df_result)
  
  if (nrow(df_result) == 0) {
    return(list(is_valid = FALSE, message = "No data available."))
  }
  
  # Extract params from JSON
  header_column     <- params$header_specs
  title_column      <- params$title
  footnote_column   <- params$footnote
  
  header_specs <- NULL
  if (!is.null(header_column)) {
    header_expr  <- rlang::parse_expr(header_column)
    header_specs <- eval(header_expr)
  }
  
  title_vec <- NULL
  if (!is.null(title_column)) {
    title_expr  <- rlang::parse_expr(title_column)
    title_vec <- eval(title_expr)
  }
  
  footnote_vec <- NULL
  if (!is.null(footnote_column)) {
    footnote_expr  <- rlang::parse_expr(footnote_column)
    footnote_vec <- eval(footnote_expr)
  }
  
  rtf_out <- tryCatch({
    create_rtf_table(df_result, specs = header_specs, header_text_lines = title_vec, footer_text_lines = footnote_vec, get_pages = TRUE)
  }, error = function(e) {
    return(list(is_valid = FALSE, message = paste("Error during creating RTF:", e$message)))
  })  
  
  message <- unlist(
    lapply(
      rtf_out[[2]], function(x) paste0(x, collapse='<br>')
      )
    )
  
  message <- gsub('\\d', 'x', message)
  
  message <- sprintf('<p style="font-family: Courier;">%s</p>', message)
  attr(message, "type") <- "rtfmt"
  
  print(message)
  
  # --- 5. Return Success ---
  return(list(is_valid = TRUE, message = message))
}
