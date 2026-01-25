# outliner

**outliner** is an R package designed to solve the issue of data processing obfuscation inside custom functions. It parses your R scripts and function definitions to generate an interactive node-graph visualization of your data pipeline using [React Flow](https://reactflow.dev/).

## Features

- **Metadata Extraction**: Define function metadata (Name, Short Description, Long Description) directly inside your R functions using a simple string syntax.
- **Pipeline Tracing**: Automatically detects assignments and pipe (`|>`) connections in your scripts to build the flow.
- **Interactive Visualization**: clearer understanding of complex data transformations through an interactive web interface.

## Installation

You can install the development version of this package locally:

```r
# Assuming you are in the package root directory
devtools::install()
```

Or load it directly for development:

```r
devtools::load_all(".")
```

## Usage

### 1. Define Your Functions with Metadata

Add a string literal as the **first expression** in your function body. Use keys starting with a colon (`:`) to define metadata.

```r
clean_data <- function(df) {
  "
  :name: Clean Data
  :short: Remove NAs and outliers
  :long: This function removes rows with missing values and filters outliers 
         based on the Z-score of the 'value' column.
  "
  # Your code here
  df[!is.na(df$value), ]
}
```

**Supported Keys:**
- `:name`: The display label for the node.
- `:short`: A brief summary shown on the node.
- `:long`: detailed description shown when expanding the node.

### 2. Write Your Analysis Script

Create a standard R script (or use an existing one) that calls your functions.

```r
# analysis_script.R
data <- read.csv("raw_data.csv")
clean <- clean_data(data)
result <- analyze_metric(clean)
```

### 3. Visualize the Flow

Use `serve_outline` to parse the script and open the visualization in your browser.

```r
library(outliner)
serve_outline("analysis_script.R")
```

### 4. Object Tracking (Optional)

To focus the visualization on specific data objects and ignore others, add a tracking header to the top of your file.

**For R Scripts:**
Use a comment header:
```r
# @outliner_track: data, model, results
```

**For RMD/QMD Files:**
Use the same comment header, or a YAML-style property in the document frontmatter:
```yaml
outliner_track: [data, model, results]
```

When this header is present, the parser will **only** generate nodes for the specified objects and any functions that interact with them, filtering out all other variables.

## Example

A reproducible example is included in `vignettes/repro_example`.

```r
# Load the package
devtools::load_all()

# Run the verification script which runs the example
source("verify_script.R")
```

## Project Structure (For Developers)

This project combines an R package with a React frontend.

```
outliner/
├── R/
│   ├── parser_meta.R   # Extracts metadata from function bodies
│   ├── parser_flow.R   # AST parser for tracking data flow in scripts
│   └── visualizer.R    # Generates JSON data and serves the web app via httpuv
├── inst/
│   ├── app/            # Source code for the React Application (Vite project)
│   │   ├── src/        # React components (App.jsx, CustomNode.jsx)
│   │   └── package.json
│   └── www/            # Compiled static assets (HTML/JS/CSS) served by R
└── vignettes/
     └── repro_example/ # Sample project for testing and verification
```

### Development Workflow

1.  **Modify R Code**: Changes in `R/` are standard. Reload with `devtools::load_all()`.
2.  **Modify Frontend**:
    -   Navigate to `inst/app` and run `npm install` if needed.
    -   Run `npm run dev` to develop locally (mock data required in `public/data.json`).
    -   Run `npm run build` to compile the app to `dist/`.
    -   **Important**: Copy the contents of `inst/app/dist` to `inst/www` so the R package can serve the updated version.
3.  **Run Verification**: Use `verifiy_script.R` to test the end-to-end flow.
