rm(list=ls())
cat('\014')

library(coastr)
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)
library(jsonlite)
library(textclean)
library(rlang)

prog_status <- 'qa'

setwd("~/")
specs_path <- 'specs/br_specs.xlsx'

# read specs ####

sheet_name <- 'Efficacy estimand'
sheet_name_friendly <- tolower(gsub('\\s+','_', trimws(sheet_name)))
specs_raw <- openxlsx::read.xlsx(specs_path, sheet=sheet_name) %>% rename_all(tolower)

# ARDS Function ----------------------------------------------------------------

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
    
    ards_data <- ards_data %>% rename_all(tolower)
    
    ref_column <- tolower(ref_column)
    trt_column <- tolower(trt_column)
    
    # In the filter query replace all variable with their lowercase versions
    
    filter_query_masked <- str_replace_all(filter_query, "'[^']*'|\"[^\"]*\"", "__STRING__")
    regex_pattern <- '\\b[[:alnum:]_\\.]+(?=\\s*(==|!=|<=|>=|<|>|%IN%|%in%))'
    vars <- str_extract_all(filter_query_masked, regex_pattern)[[1]]
    
    for (v in unique(vars)) {
      filter_query <- str_replace_all(filter_query, 
                                      paste0('\\b', v, '\\b(?=\\s*(==|!=|<=|>=|<|>|%IN%|%in%))'),
                                      tolower(v))
    }
    
    
    
    if (!measure %in% unique(ards_data[[resulttype_column]][ards_data[[ref_column]]==""])) {
      stop(sprintf("Measure %s not in:\n%s.", measure, paste(unique(ards_data[[resulttype_column]][ards_data[[ref_column]]==""]), collapse='\n')))
    }
    
    if (!all(c(difference_measure, difference_lci, difference_uci) %in% unique(ards_data[[resulttype_column]][ards_data[[ref_column]]!=""]))) {
      stop(sprintf("Difference Measure (or CIs) not in:\n%s.", paste(unique(ards_data[[resulttype_column]][ards_data[[ref_column]]!=""]), collapse='\n')))
    }
    
    filter_expr <- rlang::parse_expr(filter_query)
    
    general_query <- gsub(paste0(ref_column,"\\s*==\\s*['\"].+?['\"]"), 'TRUE', filter_query)
    general_expr <- rlang::parse_expr(sprintf("%s & %s==''", general_query, ref_column))
    
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
        cmp_value = trt_value[.data[[trt_column]]==cmp_name]
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
      mutate(cmp_name=!!cmp_name, measure=!!measure, difference_measure=!!difference_measure) %>%
      select(measure, cmp_name, cmp_value, trt_name=trt, trt_value, difference_measure, effect_estimate, effect_lower_ci, effect_upper_ci) %>%
      filter(trt_name!=!!cmp_name)
    
  }, error = function(e) {
    return(list(is_valid = FALSE, message = paste("Error during data extraction/filtering:", e$message)))
  })
  
  if (!is.data.frame(df_result)) return(df_result)
  if (nrow(df_result) == 0) return(list(is_valid = FALSE, message = "Final query resulted in zero rows."))
  
  # --- 4. Return Success ---
  return(list(is_valid = TRUE, message = df_result))
}

# ------------------------------------------------------------------------------

specs <- specs_raw %>% 
  filter(trimws(toupper(include))=='Y') %>%
  filter(!is.na(source_path)) %>%
  group_by(source_path) %>%
  mutate(groupid=row_number()) %>%
  ungroup() %>%
  rename_all(~ gsub('[.]', '_', .))

d1 <-
specs %>%
  filter(!is.na(ards_condition)) %>%
  select(groupid, ards_path, ards_condition)

all_data_lst <- list()

for (idx in 1:nrow(d1)) {
  groupid <- d1$groupid[idx]
  path <- d1$ards_path[idx]
  json_str <- replace_html(d1$ards_condition[idx])
  params <- try(fromJSON(json_str), silent=TRUE)
  
  ards_result <- perform_ards_check(path, params)
  
  if (ards_result$is_valid) {
    data <- ards_result$message %>% 
      mutate(groupid=!!groupid, ards_path=!!path)
    all_data_lst[[idx]] <- data
  } else {
    warning(ards_result$message)
  }

}

all_data <- bind_rows(all_data_lst) %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))

final_specs <-
  specs %>% 
  select(include, effect_type, estimand, variable_type, study_or_integration, timepoint,
         improvement_direction, source_path, groupid, ards_path, ards_condition, comment) %>%
  left_join(all_data, by=c('ards_path', 'groupid'))

write.csv(final_specs, sprintf("data/%s.csv", sheet_name_friendly), row.names=FALSE)
