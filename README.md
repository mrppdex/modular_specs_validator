# modular_specs_validator

## Setup
1. Create a project folder and save `app.R` in the root of that folder.
2. Add a `config.json` file to the same folder.
3. Create a sub-folder named `modules` and place your validation function scripts inside it.
4. Install the required packages:

```r
install.packages(c(
  "shiny", "shinyjs", "DT", "dplyr", "purrr", "haven", "rlang",
  "bslib", "openxlsx", "jsonlite", "htmltools", "knitr", "rmarkdown",
  "ggplot2", "plotly", "htmlwidgets", "textclean"
))
```

Pandoc must also be installed (typically provided with RStudio).

## Running the App
From the project root directory, launch the Shiny app with:

```r
shiny::runApp()
```

