# outliner

**outliner** is an annotation-first R package for documenting and visualizing analysis pipelines.

The package is built for research code that should remain valid R code even when `outliner` is not installed. Instead of runtime wrappers or special tracking functions, users add lightweight comment annotations to scripts, functions, and notebook chunks. `outliner` then parses those comments, links them to assignments and function calls, and renders the result as an interactive graph of:

- **steps**: annotated transformations, code blocks, or function internals
- **artifacts**: data objects that move through the pipeline
- **lineage**: the path of one object through successive processing steps

The goal is to reduce the obfuscation created by helper functions, make complex preprocessing pipelines inspectable, and provide a clearer public-facing overview of how a dataset changes during an analysis.

## Design Principles

- **No runtime dependency in user code**: comments decorate code, but do not change how it runs.
- **Annotation-first**: explicit comments drive the narrative shown in the graph.
- **Static fallback**: when annotations are missing, `outliner` still infers simple assignments and function calls.
- **Research-friendly output**: the frontend is meant to support both internal debugging and publishable pipeline overviews.

## Annotation Syntax

Use `# @outliner_step` comment blocks immediately before the expression or code block you want to describe.

### In functions

```r
clean_missing <- function(df) {
  # @outliner_step
  # label: Drop incomplete measurements
  # summary: Remove rows with missing values in the metric column
  # modifies: df
  # details:
  # - keep only rows where the metric is observed
  # - preserve the original columns for later normalization
  df[!is.na(df$metric), ]
}
```

### In scripts

```r
# @outliner_step
# label: Merge baseline sources
# inputs: demo, labs
# outputs: baseline
baseline <- merge(demo, labs, by = "id")
```

### In Rmd/Qmd chunks

Annotations can appear inside R code chunks in the same form:

```r
# @outliner_step
# label: Fit primary model
# inputs: analysis_df
# outputs: model_fit
# summary: Estimate the main treatment effect
model_fit <- lm(y ~ trt + age + sex, data = analysis_df)
```

### Supported fields

- `label`: step title shown in the graph
- `summary`: short description shown in the node and side panel
- `details`: bullet-like narrative lines shown in the detail panel
- `inputs`: explicit input artifacts
- `outputs`: explicit output artifacts
- `modifies`: artifacts or parameters changed internally
- `section`: optional grouping label
- `tags`: comma-separated tags
- `publish`: `true` or `false`

## Object Tracking

To focus on specific artifacts, add a tracking header to the top of the root script or document.

For an R script:

```r
# @outliner_track: raw, cleaned, final
```

For Rmd/Qmd frontmatter:

```yaml
outliner_track: [raw, cleaned, final]
```

The current frontend also lets you focus on any artifact interactively, even without the header.

## Quick Start

```r
devtools::load_all(".")
serve_outline("vignettes/repro_example/analysis.R")
```

That will:

1. parse the root analysis file
2. recursively inspect `source()`d helper files
3. expand annotated function internals into step nodes
4. open the React-based visualizer in a browser

## Local Development

### Prerequisites

- R
- `devtools` for local package loading
- Node.js and npm for the frontend

### Backend development

From the repository root:

```r
devtools::load_all(".")
```

Useful entry points:

```r
# Returns the graph payload as an R list
outline_process("vignettes/repro_example/analysis.R")

# Starts the local server and browser UI
serve_outline("vignettes/repro_example/analysis.R")
```

### Frontend development

From `inst/app`:

```bash
npm install
npm run dev
```

The Vite app reads mock data from `inst/app/public/data.json` when `window.outlinerData` is not injected by R.

To build the frontend:

```bash
npm run build
```

Then copy the build output into `inst/www` so the R package serves the latest static assets:

```powershell
Copy-Item -Path dist\* -Destination ..\www -Recurse -Force
```

## Running and Testing Locally

### Fast parser check

This exercises the static parser without starting the blocking HTTP server:

```powershell
Rscript -e "source('R/parser_meta.R'); source('R/parser_flow.R'); source('R/outline_parser.R'); source('R/visualizer.R'); str(outline_process('vignettes/repro_example/analysis.R'), max.level = 2)"
```

### End-to-end package UI

```r
devtools::load_all(".")
serve_outline("vignettes/repro_example/analysis.R")
```

### Verification script

The repository includes `verify_script.R`, which sources the package files and runs the example:

```r
source("verify_script.R")
```

Note that `serve_outline()` is a blocking server loop. Stop it with `Esc` in an interactive session.

### Frontend production build check

From `inst/app`:

```bash
npm run build
```

If the build succeeds, the React app is valid and ready to copy into `inst/www`.

## Project Structure

```text
outliner/
|- R/
|  |- outline_parser.R   # Annotation-first parser and graph builder
|  |- parser_meta.R      # Annotation parsing helpers + legacy string metadata support
|  |- parser_flow.R      # Public compatibility entry point
|  |- visualizer.R       # Graph generation and local server
|- inst/
|  |- app/               # React + Vite source
|  |- www/               # Built static app served by the R package
|- vignettes/
|  |- repro_example/     # Example analysis and helper functions
|- verify_script.R       # Local verification helper
```

## Current Scope and Limits

The new parser is intentionally less magical than a full static analysis engine.

It works best when:

- important transformations are annotated with `# @outliner_step`
- helper functions are brought in through plain `source("file.R")`
- pipelines are mostly linear assignments or pipe chains

It is still heuristic for:

- complex control flow
- deep nonstandard evaluation
- multi-expression chunk-level grouping without explicit annotations on each step

Legacy string-literal metadata inside functions is still supported as a fallback, but comment annotations are now the preferred path.
