# Specs Quality and Integrity Checker - Modular Version
#
# SETUP:
# 1. Create a project folder.
# 2. Save this file as 'app.R' in the root of that folder.
# 3. Create a 'config.json' file in the same folder.
# 4. Create a sub-folder named 'modules'.
# 5. Save your validation function scripts inside 'modules'.
# 6. Install required packages:
#    install.packages(c("shiny", "shinyjs", "DT", "dplyr", "purrr", "haven", "rlang", "bslib", "openxlsx", "jsonlite", "htmltools", "knitr", "rmarkdown", "ggplot2", "plotly", "htmlwidgets", "textclean"))
# 7. Make sure Pandoc is installed on the system running the app. It is usually installed with RStudio.
# 8. Run the app from the project root directory.

library(shiny)
library(shinyjs)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(haven)
library(rlang)
library(bslib)
library(openxlsx)
library(jsonlite)
library(htmltools)
library(knitr)
library(textclean)
library(rmarkdown) # For document previews
library(ggplot2)   # For ggplot visualizations
library(plotly)    # For plotly visualizations
library(htmlwidgets) # For saving widgets to HTML

# --- Load Configuration at Startup ---
config <- try(fromJSON("config.json", simplifyDataFrame = FALSE), silent = TRUE)
if (inherits(config, "try-error")) {
  stop("Could not find or parse config.json. Please ensure it exists and is valid.")
}

# ==============================================================================
# UI Definition
# ==============================================================================
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "spacelab"),
  tags$head(tags$style(HTML("
    .dataTables_scrollBody td {
      max-height: 60px;
      overflow-y: auto;
      white-space: pre-wrap;
      word-wrap: break-word;
    }
    .popover {
      max-width: 850px !important; /* Increased for plots */
    }
    /* Ensure modal appears above everything */
    .modal {
      z-index: 2000 !important;
    }
    .modal-backdrop {
      z-index: 1999 !important;
    }
  "))),
  title = "Specs Quality and Integrity Checker",
  
  sidebar = sidebar(
    accordion(
      open = TRUE,
      accordion_panel(
        "Step 1: Specifications",
        textInput("spec_col_name", "Column Name / Rule Name"),
        selectInput("spec_col_type", "Data Type",
                    choices = setNames(map_chr(config$data_types, "id"), map_chr(config$data_types, "label"))),
        uiOutput("dynamic_spec_ui"),
        actionButton("add_spec", "Add Spec Rule", icon = icon("plus")),
        hr(),
        fileInput("upload_specs", "Upload Spec CSV", accept = ".csv"),
        downloadButton("download_specs", "Download Specs as CSV")
      ),
      accordion_panel(
        "Step 2: Upload & Validate",
        fileInput("upload_excel", "Upload Excel File", accept = c(".xlsx")),
        uiOutput("sheet_selector_ui"),
        actionButton("validate_btn", "Validate Selected Sheets", icon = icon("check"), class = "btn-primary")
      )
    )
  ),
  
  navset_card_tab(
    id = "main_content_tabs",
    nav_panel("Specification Rules", DTOutput("spec_table")),
    nav_panel("Validation Results", uiOutput("validation_tabs")),
    nav_panel("Error Summary",
      DTOutput("error_summary_table"),
      br(),
      uiOutput("download_errors_ui")
    )
  ),
  # Add a container for the dynamically generated modals
  uiOutput("modal_container")
)

# ==============================================================================
# Server Logic
# ==============================================================================
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    specs = tibble(Name = character(), Type = character(), Values = character()),
    uploaded_excel_data = list(),
    validation_results = list(),
    error_summary = tibble(),
    modal_htmls = list() # To store UI for document preview modals
  )
  
  # --- Dynamic UI Generation ---
  output$dynamic_spec_ui <- renderUI({
    req(input$spec_col_type)
    selected_type <- purrr::detect(config$data_types, ~.x$id == input$spec_col_type)
    
    if (is.null(selected_type$ui_elements)) return(NULL)
    
    purrr::map(selected_type$ui_elements, ~ {
      if (.x$type == "textAreaInput") {
        textAreaInput(.x$id, .x$label)
      } else if (.x$type == "textInput") {
        textInput(.x$id, .x$label, placeholder = .x$placeholder)
      }
    })
  })

  # --- Spec Management Logic ---
  observeEvent(input$add_spec, {
    req(input$spec_col_name, input$spec_col_type)
    
    if(input$spec_col_name %in% rv$specs$Name){
        showNotification("A rule for this column name already exists.", type = "warning")
        return()
    }
    
    selected_type <- purrr::detect(config$data_types, ~.x$id == input$spec_col_type)
    
    spec_values <- NA_character_
    if (!is.null(selected_type$value_constructor)) {
      value_parts <- purrr::map_chr(selected_type$value_constructor, ~{
        paste0(.x$key, "=", input[[.x$id]])
      })
      spec_values <- paste(value_parts, collapse = ";")
    }

    new_rule <- tibble(Name = input$spec_col_name, Type = input$spec_col_type, Values = spec_values)
    rv$specs <- bind_rows(rv$specs, new_rule)

    updateTextInput(session, "spec_col_name", value = "")
    if (!is.null(selected_type$ui_elements)) {
      purrr::walk(selected_type$ui_elements, ~ {
        if (.x$type == "textAreaInput") updateTextAreaInput(session, .x$id, value = "")
        if (.x$type == "textInput") updateTextInput(session, .x$id, value = "")
      })
    }
  })

  observeEvent(input$upload_specs, {
    req(input$upload_specs)
    df <- try(read.csv(input$upload_specs$datapath, stringsAsFactors = FALSE, check.names = FALSE))
    if(inherits(df, "try-error")){
        showNotification("Failed to read the spec file. Please ensure it's a valid CSV.", type = "error")
        return()
    }
    if(!all(c("Name", "Type", "Values") %in% colnames(df))){
        showNotification("Spec file must contain 'Name', 'Type', and 'Values' columns.", type = "error")
        rv$specs <- tibble(Name = character(), Type = character(), Values = character())
    } else {
        rv$specs <- as_tibble(df)
        showNotification("Specifications loaded successfully.", type = "message")
    }
  })
  
  output$spec_table <- renderDT({
    datatable(rv$specs, options = list(pageLength = 10, dom = 'tip'), rownames = FALSE, selection = 'none')
  })
  
  output$download_specs <- downloadHandler(
    filename = function() { paste0("data-specs-", Sys.Date(), ".csv") },
    content = function(file) { write.csv(rv$specs, file, row.names = FALSE) }
  )
  
  # --- Data Upload Logic ---
  observeEvent(input$upload_excel, {
    req(input$upload_excel)
    path <- input$upload_excel$datapath
    tryCatch({
      sheet_names <- openxlsx::getSheetNames(path)
      rv$uploaded_excel_data <- set_names(map(sheet_names, ~openxlsx::read.xlsx(path, sheet = .x)), sheet_names)
      output$sheet_selector_ui <- renderUI({
        checkboxGroupInput("selected_sheets", "Select Sheets to Validate:", choices = sheet_names, selected = sheet_names)
      })
      showNotification(paste("Excel file loaded with", length(sheet_names), "sheets."), type = "message")
    }, error = function(e) {
      showNotification(paste("Error reading Excel file:", e$message), type = "error")
      output$sheet_selector_ui <- renderUI({ helpText("Could not read the uploaded file.") })
    })
  })
  
  # --- Core Validation Function ---
  run_validation <- function() {
    req(input$selected_sheets, nrow(rv$specs) > 0, length(rv$uploaded_excel_data) > 0)
    
    selected <- input$selected_sheets
    specs <- rv$specs
    all_modals <- list() # Store modals for all sheets in this run
    
    withProgress(message = 'Validating data...', value = 0, {
      results <- map(selected, function(sheet_name) {
        if (!sheet_name %in% names(rv$uploaded_excel_data)) return(NULL)
        incProgress(1/length(selected), detail = paste("Processing sheet:", sheet_name))
        data_sheet <- rv$uploaded_excel_data[[sheet_name]]
        
        # Create a character matrix for rendering, escaping original data to prevent XSS
        render_matrix <- as.data.frame(lapply(data_sheet, function(c) htmltools::htmlEscape(as.character(c))), stringsAsFactors = FALSE)
        colnames(render_matrix) <- colnames(data_sheet)
        
        error_matrix <- matrix(NA_character_, nrow = nrow(data_sheet), ncol = ncol(data_sheet))
        popover_content_matrix <- matrix(NA_character_, nrow = nrow(data_sheet), ncol = ncol(data_sheet))
        popover_title_matrix <- matrix(NA_character_, nrow = nrow(data_sheet), ncol = ncol(data_sheet))
        colnames(error_matrix) <- colnames(popover_content_matrix) <- colnames(popover_title_matrix) <- colnames(data_sheet)
        
        append_msg <- function(existing_msg, new_msg) {
          if (is.na(existing_msg)) return(new_msg) else paste(existing_msg, new_msg, sep = " | ")
        }

        get_param <- function(params_str, p_name) {
          val <- strsplit(params_str, ";")[[1]] %>% detect(~startsWith(.x, paste0(p_name, "=")))
          if (is.null(val)) return(NA_character_)
          sub(paste0(p_name, "="), "", val)
        }

        for (spec_rule_row in 1:nrow(specs)) {
            rule <- specs[spec_rule_row, ]
            selected_type <- purrr::detect(config$data_types, ~.x$id == rule$Type)
            
            if(selected_type$category == "standard") {
                col_name <- rule$Name
                if (col_name %in% colnames(data_sheet)) {
                    col_idx <- which(colnames(data_sheet) == col_name)
                    for (row_idx in 1:nrow(data_sheet)) {
                        value <- data_sheet[[col_name]][row_idx]
                        validation_output <- source("modules/standard_validators.R")$value(value, rule)
                        if (!validation_output$is_valid) {
                            error_msg <- validation_output$message
                            error_matrix[row_idx, col_idx] <- append_msg(error_matrix[row_idx, col_idx], error_msg)
                            popover_content_matrix[row_idx, col_idx] <- append_msg(popover_content_matrix[row_idx, col_idx], error_msg)
                            popover_title_matrix[row_idx, col_idx] <- append_msg(popover_title_matrix[row_idx, col_idx], "Validation Error")
                        } else {
                            if (rule$Type == "File Path" && !is.na(value) && value != "") {
                                ext <- tolower(tools::file_ext(value))
                                if (ext %in% c("docx", "doc", "rtf") && file.exists(value)) {
                                    html_file <- tempfile(fileext = ".html")
                                    try(pandoc_convert(value, to = "html5", output = html_file), silent = TRUE)
                                    
                                    if(file.exists(html_file)){
                                        preview_html <- paste(readLines(html_file, warn=FALSE), collapse="\n")
                                        modal_id <- paste("preview-modal", make.names(sheet_name), row_idx, col_idx, sep="-")
                                        modal_ui <- tags$div(
                                            class = "modal fade", id = modal_id, tabindex = "-1",
                                            `aria-labelledby` = paste0(modal_id, "-label"), `aria-hidden` = "true",
                                            tags$div(class = "modal-dialog modal-xl modal-dialog-scrollable",
                                                tags$div(class = "modal-content",
                                                    tags$div(class = "modal-header",
                                                        tags$h5(class = "modal-title", id = paste0(modal_id, "-label"), basename(value)),
                                                        tags$button(type = "button", class = "btn-close", `data-bs-dismiss` = "modal", `aria-label` = "Close")
                                                    ),
                                                    tags$div(class = "modal-body", HTML(preview_html))
                                                )
                                            )
                                        )
                                        all_modals[[modal_id]] <<- modal_ui
                                        button_html <- as.character(tags$button(
                                            type = "button",
                                            class = "btn btn-sm btn-outline-secondary py-0",
                                            `data-bs-toggle` = "modal",
                                            `data-bs-target` = paste0("#", modal_id),
                                            "View Document"
                                        ))
                                        render_matrix[row_idx, col_idx] <- paste(render_matrix[row_idx, col_idx], button_html, sep="<br>")
                                    }
                                }
                            }
                        }
                    }
                }
            } else if (selected_type$category == "complex") {
                path_col_name <- get_param(rule$Values, "path_col")
                filter_col_name <- get_param(rule$Values, "filter_col")
                if (is.na(path_col_name) || is.na(filter_col_name) || !all(c(path_col_name, filter_col_name) %in% colnames(data_sheet))) next
                
                filter_col_idx <- which(colnames(data_sheet) == filter_col_name)

                for (row_idx in 1:nrow(data_sheet)) {
                    path_value <- data_sheet[[path_col_name]][row_idx]
                    json_str <- replace_html(data_sheet[[filter_col_name]][row_idx])
                    
                    if (is.na(path_value) || path_value == "" || is.na(json_str) || json_str == "") next

                    json_params <- try(fromJSON(json_str), silent = TRUE)
                    if(inherits(json_params, "try-error")) {
                        error_matrix[row_idx, filter_col_idx] <- append_msg(error_matrix[row_idx, filter_col_idx], "Invalid JSON format.")
                        popover_content_matrix[row_idx, filter_col_idx] <- append_msg(popover_content_matrix[row_idx, filter_col_idx], "Invalid JSON format in cell.")
                        popover_title_matrix[row_idx, filter_col_idx] <- append_msg(popover_title_matrix[row_idx, filter_col_idx], "Validation Error")
                        next
                    }
                    
                    function_name <- json_params$validation_module
                    if(is.null(function_name) || !function_name %in% names(config$function_mapping)) {
                        let_msg <- paste("Module", function_name, "not defined in config.json.")
                        error_matrix[row_idx, filter_col_idx] <- append_msg(error_matrix[row_idx, filter_col_idx], let_msg)
                        popover_content_matrix[row_idx, filter_col_idx] <- append_msg(popover_content_matrix[row_idx, filter_col_idx], let_msg)
                        popover_title_matrix[row_idx, filter_col_idx] <- append_msg(popover_title_matrix[row_idx, filter_col_idx], "Configuration Error")
                        next
                    }
                    
                    module_path <- config$function_mapping[[function_name]]
                    if(!file.exists(module_path)) {
                        let_msg <- paste("Module file not found:", module_path)
                        error_matrix[row_idx, filter_col_idx] <- append_msg(error_matrix[row_idx, filter_col_idx], let_msg)
                        popover_content_matrix[row_idx, filter_col_idx] <- append_msg(popover_content_matrix[row_idx, filter_col_idx], let_msg)
                        popover_title_matrix[row_idx, filter_col_idx] <- append_msg(popover_title_matrix[row_idx, filter_col_idx], "Configuration Error")
                        next
                    }
                    
                    module_env <- new.env()
                    source(module_path, local = module_env)
                    
                    if(!exists(function_name, envir = module_env)) {
                       let_msg <- paste("Function", function_name, "not found in module", module_path)
                       error_matrix[row_idx, filter_col_idx] <- append_msg(error_matrix[row_idx, filter_col_idx], let_msg)
                       popover_content_matrix[row_idx, filter_col_idx] <- append_msg(popover_content_matrix[row_idx, filter_col_idx], let_msg)
                       popover_title_matrix[row_idx, filter_col_idx] <- append_msg(popover_title_matrix[row_idx, filter_col_idx], "Configuration Error")
                       next
                    }
                    
                    validation_func <- module_env[[function_name]]
                    validation_output <- validation_func(path_value, json_params)
                    
                    # --- MODIFIED: Handle plots, data frames, and text ---
                    if (inherits(validation_output$message, "plotly") || inherits(validation_output$message, "htmlwidget")) {
                        tmp_html <- tempfile(fileext = ".html")
                        htmlwidgets::saveWidget(validation_output$message, tmp_html, selfcontained = TRUE)
                        widget_html <- paste(readLines(tmp_html, warn = FALSE), collapse = "\n")
                        wrapper_div <- as.character(tags$iframe(
                            srcdoc = widget_html,
                            style = "width: 750px; height: 450px; border: none;",
                            seamless = "seamless"
                        ))
                        popover_content_matrix[row_idx, filter_col_idx] <- append_msg(popover_content_matrix[row_idx, filter_col_idx], wrapper_div)
                        popover_title_matrix[row_idx, filter_col_idx] <- append_msg(popover_title_matrix[row_idx, filter_col_idx], "Plot Visualization")

                    } else if (inherits(validation_output$message, "ggplot")) {
                        tmp_file <- tempfile(fileext = ".png")
                        tryCatch({
                            ggsave(tmp_file, plot = validation_output$message, width = 7, height = 5, units = "in", dpi = 150)
                            img_uri <- knitr::image_uri(tmp_file)
                            wrapper_div <- as.character(tags$div(
                                style = "max-width: 750px; max-height: 500px; overflow: auto;",
                                tags$img(src = img_uri, style = "width: 100%; height: auto;")
                            ))
                            popover_content_matrix[row_idx, filter_col_idx] <- append_msg(popover_content_matrix[row_idx, filter_col_idx], wrapper_div)
                            popover_title_matrix[row_idx, filter_col_idx] <- append_msg(popover_title_matrix[row_idx, filter_col_idx], "Plot Visualization")
                        }, error = function(e) {
                            err_msg <- paste("Failed to render ggplot:", e$message)
                            popover_content_matrix[row_idx, filter_col_idx] <- append_msg(popover_content_matrix[row_idx, filter_col_idx], err_msg)
                            popover_title_matrix[row_idx, filter_col_idx] <- append_msg(popover_title_matrix[row_idx, filter_col_idx], "Render Error")
                        })
                    } else if(is.data.frame(validation_output$message)) {
                        html_table <- knitr::kable(validation_output$message, format = "html", table.attr = "class='table table-sm table-bordered' style='margin-bottom:0;'")
                        wrapper_div <- as.character(tags$div(style = "max-width: 800px; max-height: 400px; overflow: auto; background-color: white; color: black;", HTML(html_table)))
                        popover_content_matrix[row_idx, filter_col_idx] <- append_msg(popover_content_matrix[row_idx, filter_col_idx], wrapper_div)
                        popover_title_matrix[row_idx, filter_col_idx] <- append_msg(popover_title_matrix[row_idx, filter_col_idx], "Data Check Result")
                    } else if (is.character(validation_output$message) && !is.na(validation_output$message) && validation_output$message != "") {
                        popover_content_matrix[row_idx, filter_col_idx] <- append_msg(popover_content_matrix[row_idx, filter_col_idx], validation_output$message)
                        popover_title_matrix[row_idx, filter_col_idx] <- append_msg(popover_title_matrix[row_idx, filter_col_idx], "Validation Info")
                    }

                    if (!validation_output$is_valid) {
                        error_msg <- "Complex validation failed."
                        if(is.data.frame(validation_output$message) && nrow(validation_output$message) > 0) {
                            error_msg <- "Multiple issues found (see popover for details)."
                        } else if (is.character(validation_output$message) && !is.na(validation_output$message) && validation_output$message != "") {
                            error_msg <- str_trunc(validation_output$message, 100)
                        } else if (inherits(validation_output$message, "ggplot") || inherits(validation_output$message, "plotly")) {
                           error_msg <- "Validation produced a plot (see popover)."
                        }
                        error_matrix[row_idx, filter_col_idx] <- append_msg(error_matrix[row_idx, filter_col_idx], error_msg)
                    }
                }
            }
        }
        
        return(list(
            data = data_sheet,
            render_matrix = render_matrix,
            error_matrix = error_matrix,
            popover_content = popover_content_matrix,
            popover_title = popover_title_matrix
        ))
      })
    }) # End withProgress
    
    results <- results[!sapply(results, is.null)]
    rv$validation_results <- set_names(results, selected[selected %in% names(rv$uploaded_excel_data)])
    rv$modal_htmls <- all_modals

    error_summary_df <- imap_dfr(rv$validation_results, ~{
      error_matrix <- .x$error_matrix
      error_indices <- which(!is.na(error_matrix), arr.ind = TRUE)
      if(nrow(error_indices) > 0) {
        map_dfr(1:nrow(error_indices), function(i) {
          row_idx <- error_indices[i, "row"]; col_idx <- error_indices[i, "col"]
          tibble(Sheet = .y, Row = row_idx, Column = colnames(.x$data)[col_idx],
                 Value = as.character(.x$data[row_idx, col_idx]), Reason = error_matrix[row_idx, col_idx])
        })
      } else { tibble() }
    })
    rv$error_summary <- error_summary_df

    if(!is.null(shiny::getDefaultReactiveDomain())) { showNotification("Validation complete!", type = "message") }
  }

  # --- Event Triggers ---
  observeEvent(input$validate_btn, { run_validation() })
  
  # --- UI Rendering ---
  
  output$modal_container <- renderUI({
    tagList(unname(rv$modal_htmls))
  })

  output$validation_tabs <- renderUI({
    if (length(rv$validation_results) == 0) {
      return(helpText("Validation results will appear here."))
    }
    
    sheet_tabs <- imap(rv$validation_results, ~{
      tabPanel(title = .y, DTOutput(paste0("table_", .y)))
    })
    
    do.call(tabsetPanel, c(id="sheet_tabs_panel", unname(sheet_tabs)))
  })
  
  output$error_summary_table <- renderDT({
    if (nrow(rv$error_summary) == 0 && length(rv$validation_results) > 0) {
      return(datatable(data.frame(Message = "No validation errors found."), rownames = FALSE, options = list(dom = 't')))
    }
    req(nrow(rv$error_summary) > 0)
    datatable(rv$error_summary, rownames = FALSE, filter = 'top', selection = 'none',
              extensions = 'FixedHeader',
              options = list(pageLength = 10, scrollX = TRUE, dom = 'frtip', fixedHeader = TRUE))
  })

  output$download_errors_ui <- renderUI({
    req(nrow(rv$error_summary) > 0)
    downloadButton("download_error_summary_btn", "Download Full Error Report")
  })
  
  output$download_error_summary_btn <- downloadHandler(
    filename = function() { paste0("validation-error-summary-", Sys.Date(), ".xlsx") },
    content = function(file) {
      req(nrow(rv$error_summary) > 0)
      sorted_summary <- rv$error_summary %>% arrange(Sheet, as.numeric(Row), Column)
      wb <- createWorkbook()
      addWorksheet(wb, "Error Summary")
      writeData(wb, "Error Summary", "Specs Quality and Integrity Checker", startCol = 1, startRow = 1)
      mergeCells(wb, "Error Summary", cols = 1:ncol(sorted_summary), rows = 1)
      titleStyle <- createStyle(fontSize = 14, textDecoration = "bold", halign = "center")
      addStyle(wb, "Error Summary", style = titleStyle, rows = 1, cols = 1)
      writeData(wb, "Error Summary", sorted_summary, startRow = 3)
      headerStyle <- createStyle(textDecoration = "bold")
      addStyle(wb, "Error Summary", style = headerStyle, rows = 3, cols = 1:ncol(sorted_summary), gridExpand = TRUE)
      setColWidths(wb, "Error Summary", cols = 1:ncol(sorted_summary), widths = "auto")
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # --- Dynamic Observers for DT tables ---
  observe({
    req(length(rv$validation_results) > 0)
    walk(names(rv$validation_results), function(sheet_name) {
      
      output[[paste0("table_", sheet_name)]] <- renderDT({
        res <- rv$validation_results[[sheet_name]]
        dt <- datatable(res$render_matrix, rownames = FALSE, escape = FALSE, selection = 'none',
          extensions = 'FixedHeader',
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            scrollY = "calc(100vh - 400px)",
            fixedHeader = TRUE,
            rowCallback = JS(
              "function(row, data, index) {",
              "  var errorMatrix = ", jsonlite::toJSON(res$error_matrix, na = "null"), ";",
              "  var popoverContentMatrix = ", jsonlite::toJSON(res$popover_content, na = "null"), ";",
              "  var popoverTitleMatrix = ", jsonlite::toJSON(res$popover_title, na = "null"), ";",
              "  for (var j=0; j < data.length; j++) {",
              "    var cell = $(row).find('td').eq(j);",
              "    if (popoverContentMatrix[index] && popoverContentMatrix[index][j] !== null) {",
              "      $(cell).attr('data-bs-toggle', 'popover')",
              "             .attr('data-bs-html', 'true')",
              "             .attr('data-bs-trigger', 'click')",
              "             .attr('data-bs-title', popoverTitleMatrix[index][j])",
              "             .attr('data-bs-content', popoverContentMatrix[index][j]);",
              "    }",
              "    if (errorMatrix[index] && errorMatrix[index][j] !== null) {",
              "      cell.css('background-color', 'rgba(255, 135, 135, 0.7)');",
              "    }",
              "  }",
              "}"
            ),
            drawCallback = JS(
              "function(settings) {",
              "  var allowlist = bootstrap.Popover.Default.allowList;",
              "  allowlist.table = []; allowlist.thead = []; allowlist.tbody = []; allowlist.tr = [];",
              "  allowlist.td = ['style']; allowlist.th = ['style']; allowlist.div = ['style'];",
              "  allowlist.p = []; allowlist.h1 = []; allowlist.h2 = []; allowlist.h3 = [];",
              "  allowlist.ul = []; allowlist.ol = []; allowlist.li = [];",
              "  allowlist.strong = []; allowlist.em = [];",
              "  allowlist.br = [];",
              // --- MODIFIED: Add iframe support for Plotly ---
              "  allowlist.iframe = ['srcdoc', 'style', 'seamless', 'width', 'height', 'frameborder'];",
              "  allowlist.img = ['src', 'style', 'width', 'height'];",

              "  var table = this.api().table();",
              "  $(table.body()).find('[data-bs-toggle=\"popover\"]').each(function() {",
              "    var popover = bootstrap.Popover.getInstance(this);",
              "    if (popover) { popover.dispose(); }",
              "  });",
              "  $(table.body()).find('[data-bs-toggle=\"popover\"]').each(function() {",
              "    new bootstrap.Popover(this, { html: true, container: 'body', sanitize: false });",
              "  });",
              "}"
            )
          )
        )
        dt
      })
    })
  })
}

shinyApp(ui = ui, server = server)
