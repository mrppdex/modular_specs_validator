library(openxlsx)

# Create a workbook
wb <- createWorkbook()

# --- ARDS Check Data ---
ards_data <- data.frame(
  Path = c("test_data/ards_data.csv", "", ""),
  Filter = c(
    '{"validation_module":"perform_ards_check", "filter":"reftrt == \'\'", "measure":"mean", "difference_measure":"diff", "difference_lci":"lci", "difference_uci":"uci", "cmp_name":"Placebo", "ref_column":"reftrt", "trt_column":"trt", "resulttype_column":"resulttype", "result_column":"result"}',
    "",
    ""
  )
)
addWorksheet(wb, "ards_check_data")
writeData(wb, "ards_check_data", ards_data)

# --- Plot Effect Data ---
plot_effect_data <- data.frame(
  Path = c("test_data/ards_data.csv", "", ""),
  Filter = c(
    '{"validation_module":"plot_effect", "filter":"reftrt == \'\'", "measure":"mean", "difference_measure":"diff", "difference_lci":"lci", "difference_uci":"uci", "cmp_name":"Placebo", "ref_column":"reftrt", "trt_column":"trt", "resulttype_column":"resulttype", "result_column":"result"}',
    "",
    ""
  )
)
addWorksheet(wb, "plot_effect_data")
writeData(wb, "plot_effect_data", plot_effect_data)

# --- Volcano Plot Data ---
volcano_plot_data <- data.frame(
  Path = c("test_data/volcano_data.csv", "", ""),
  Filter = c(
    '{"validation_module":"volcano_plot", "filter":"TRT == \'Treatment\'", "pt_column":"PT", "trt_column":"TRT", "result_column":"Result", "resulttype_column":"ResultType", "pval_name":"PValue", "effect_name":"Effect"}',
    "",
    ""
  )
)

addWorksheet(wb, "volcano_plot_data")
writeData(wb, "volcano_plot_data", volcano_plot_data)

# Save the workbook
saveWorkbook(wb, "/Users/mrppdex/projects/R/modular_specs_validator/test_data/data.xlsx", overwrite = TRUE)
