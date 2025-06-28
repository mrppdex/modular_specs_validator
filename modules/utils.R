#' Lowercase variable names in a filter query string.
#'
#' This function takes a string containing a filter query and converts
#' all variable names to lowercase. It is designed to ignore string literals,
#' function names, and R keywords.
#'
#' @param filter_query A character string with the filter query.
#' @return The filter query with variable names in lowercase.
lowercase_query_vars <- function(filter_query) {
    # Mask string literals to avoid changing their content
    filter_query_masked <- stringr::str_replace_all(filter_query, "'[^']*'|\"[^\"]*\"", "__STRING__")

    # Regex to identify names (variables, functions, etc.)
    # An identifier starts with a letter or a dot.
    # This is a simplified version for names in a filter query.
    name_pattern <- "\\b[a-zA-Z._][a-zA-Z0-9._]*\\b"

    # Extract all name-like tokens
    all_names <- stringr::str_extract_all(filter_query_masked, name_pattern)[[1]]

    # Extract names that are followed by '(', which we assume are functions
    function_names <- stringr::str_extract_all(filter_query_masked, paste0(name_pattern, "(?=\\s*\\()"))[[1]]

    # Variables are names that are not functions
    variable_names <- setdiff(all_names, function_names)

    # Define R keywords that should not be lowercased
    r_keywords <- c("if", "else", "for", "in", "while", "repeat", "function",
                    "TRUE", "FALSE", "NULL", "NA", "Inf", "NaN",
                    "break", "next", "return")
    
    # Remove keywords from the list of variables
    variable_names <- setdiff(variable_names, r_keywords)

    # Lowercase each variable name in the original query
    for (var in unique(variable_names)) {
        # Use word boundaries to ensure whole-word replacement
        filter_query <- stringr::str_replace_all(filter_query, paste0("\\b", var, "\\b"), tolower(var))
    }

    return(filter_query)
}

#lowercase_query_vars("VAR1=='AASDF' & !VAR2 & is.na(VAR3) | VAR4 %in% c('A', 'b') & VAR5 > VAR6")
