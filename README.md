# Modular Specs Validator

This Shiny application is designed to validate data based on a set of specifications. It's modular, allowing you to define your own validation functions and integrate them easily.

## Features

*   **Dynamic Specification Rules:** Define validation rules through a user-friendly interface or by uploading a CSV file.
*   **Modular Validation:** Create custom validation functions in R and link them through a configuration file.
*   **Support for Various Data Types:** Includes standard checks for text, numeric, categorical, file paths, and regex patterns.
*   **Complex Validation Logic:** Supports complex validation scenarios that require custom logic and can return plots, tables, or text messages.
*   **Interactive Results:** View validation results in an interactive table, with popovers for detailed error messages, plots, and data frames.
*   **Error Summary:** Get a summary of all validation errors, which can be downloaded as an Excel report.

## Setup

1.  **Project Structure:**
    ```
    /your_project_folder
    ├── app.R
    ├── config.json
    ├── modules/
    │   ├── custom_validator_1.R
    │   └── custom_validator_2.R
    └── test_data/
        ├── ...
    ```

2.  **Install Dependencies:**
    Make sure you have the following R packages installed:
    ```R
    install.packages(c("shiny", "shinyjs", "DT", "dplyr", "purrr", "haven", "rlang", "bslib", "openxlsx", "jsonlite", "htmltools", "knitr", "rmarkdown", "ggplot2", "plotly", "htmlwidgets", "textclean"))
    ```

3.  **Pandoc:**
    Ensure Pandoc is installed on your system. It is usually installed with RStudio.

## Configuration (`config.json`)

The `config.json` file defines the data types and the mapping to your custom validation functions.

### `data_types`

This array defines the types of validation rules you can create. You can define `standard` types with simple UI elements or `complex` types for more advanced scenarios.

### `function_mapping`

This section maps your custom validation functions to an `id` that can be used in your data.

Each function mapping has the following properties:

*   **`id`**: A unique identifier for the function. This is used in your data's JSON configuration to specify which validation to run.
*   **`path`**: The path to the R script containing the validation function.
*   **`label`**: A user-friendly label for the button that appears in the validation results.
*   **`args`**:
    *   `required`: A list of required arguments for the function.
    *   `optional`: A list of optional arguments with their default values.

**Example:**
```json
"function_mapping": {
  "perform_ards_check": {
    "id": "ards_check",
    "path": "modules/perform_ards_check.R",
    "label": "BR data",
    "args": {
      "required": ["filter"],
      "optional": { "measure": "NA" }
    }
  }
}
```
In this example, the function `perform_ards_check` (the key) is located in `modules/perform_ards_check.R`. It is identified by `ards_check`.

## Defining Specifications (`specs.csv`)

The `specs.csv` file defines the validation rules to be applied to your data. It has three columns: `Name`, `Type`, and `Values`.

*   **`Name`**: A descriptive name for the rule. For `DataCheck` types, this name is just a label and is not used for function matching.
*   **`Type`**: The type of validation, which must match an `id` from the `data_types` section in `config.json`.
*   **`Values`**: Parameters for the validation rule, such as allowed values for a categorical type or column mappings for a complex data check.

## How to Run the App

1.  Open `app.R` in RStudio.
2.  Click "Run App".

## How Complex Validation Works

For `DataCheck` rules, the application looks for a column containing JSON configuration. This JSON specifies which validation module to run and what parameters to use.

The `validation_module` key in the JSON **must match the `id`** of a function in the `function_mapping` section of `config.json`.

**Example Data:**

In your Excel file, you might have a `Filter` column with the following JSON:

```json
{
  "validation_module": "ards_check",
  "filter": "reftrt == ''"
}
```

When the validator processes this row, it will:
1.  Find the function configuration with the `id` "ards_check" in `config.json`.
2.  Load the corresponding R script (`modules/perform_ards_check.R`).
3.  Call the `perform_ards_check` function with the parameters from the JSON.
4.  Display the results.
