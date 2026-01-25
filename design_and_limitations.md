# Design Analysis: Outliner for R

## Core Concept
The goal is to visualize data processing pipelines by parsing R code and extracting metadata from custom function "docstrings".

## Key Limitations & Necessary Changes for R

### 1. Function Metadata (The "Docstring" Issue)
**Problem:** R does not have built-in support for Python-style docstrings (strings that are accessible via `__doc__`).
**Solution:** We will enforce a convention where the *first expression* in the function body must be a string literal containing the metadata in a YAML-like format.
**Proposed Syntax:**
```r
clean_data <- function(df) {
  "
  :name: Clean Data
  :short: Removes NA values
  :long: This function filters out rows with missing values in the 'id' column.
  "
  # ... code ...
}
```
**Parsing Strategy:** use `body(fun)[[2]]` (since `[[1]]` is `{`) to check if it's a character string.

### 2. Pipeline Tracking (The "Path" Issue)
**Problem:** R scripts are often loose collections of commands. Tracing "what happened to object X" is complex if the user overwrites variables (e.g., `df <- func(df)`).
**Approach:** 
- **Static Analysis:** We will parse the Abstract Syntax Tree (AST) of the script.
- **Heuristic:** We look for assignment operations `target <- function_name(source, ...)` or pipe operations `source |> function_name(...)`.
- **Limitation:** Complex control flow (loops, if-else) might break the linear visualization. We will initially support linear pipelines (scripts/rmd chunks).

### 3. Integration with XYFlow (React)
**Problem:** R is not a web and serving a React app requires a bridge.
**Solution:**
- **Frontend:** A standalone React build (bundled) included in the R package (in `inst/www`).
- **Backend:** The R package will provide a `serve_outline()` function.
- **Bridge:** We will use `httpuv` (or `servr`) to host the static React files and provide an API endpoint (e.g., `/api/graph`) that returns the JSON graph data.

### 4. Quarto/RMarkdown Support
**Problem:** `.qmd` and `.Rmd` files mix text and code.
**Solution:** Use the `parsermd` package (or regular expressions if we want to minimize dependencies) to extract code chunks. We will treat the sequence of chunks as a linear script for the definition of the "path".

## Proposed Project Structure
```
outliner/
├── R/
│   ├── parser_function.R  # Extracts metadata from functions
│   ├── parser_script.R    # Parses scripts for data flow
│   ├── server.R           # httpuv server to host the viz
│   └── visualizer.R       # Main entry point
├── inst/
│   └── www/               # Compiled React App (XYFlow)
└── vignettes/
    └── repro_example/     # The simple example project
```
