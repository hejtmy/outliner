# Project Context: outliner

## Purpose
**outliner** is an annotation-first R package for visualizing and documenting data-processing pipelines.

The project is aimed at research workflows where code should still run normally without `outliner`. Users add `# @outliner_step` comment blocks to functions, scripts, and notebook chunks. The package then parses those annotations, links them to assignments and function calls, and renders a graph of steps and artifacts.

## Key Rules
- Do not introduce runtime wrappers into user code. Prefer comment annotations over helper functions.
- The canonical annotation syntax is `# @outliner_step` with optional fields such as `label`, `summary`, `details`, `inputs`, `outputs`, and `modifies`.
- The parser is intentionally static and heuristic. It should favor explicit annotations over clever inference.
- The frontend should emphasize inspectability: step list, artifact list, lineage focus, code excerpts, and source locations.

## Project Structure
- **`R/`**: Backend logic.
  - `outline_parser.R`: Main annotation-first parser and graph builder.
  - `parser_meta.R`: Comment annotation helpers and legacy string-metadata support.
  - `parser_flow.R`: Compatibility entry point that delegates to the new parser.
  - `visualizer.R`: Produces graph payloads and serves the React app with `httpuv`.
- **`inst/app/`**: Frontend source code (React + Vite).
  - `src/App.jsx`: Main application shell, lineage mode, side panels, detail panel.
  - `src/CustomNode.jsx`: React Flow node renderer for step and artifact nodes.
- **`inst/www/`**: Built frontend assets served by the R package.
- **`vignettes/repro_example/`**: Local example pipeline used for verification.
- **`verify_script.R`**: Simple local verification helper.

## Local Workflow
1. Modify backend code in `R/` and reload with `devtools::load_all(".")`.
2. For parser-only checks, run:
   `Rscript -e "source('R/parser_meta.R'); source('R/parser_flow.R'); source('R/outline_parser.R'); source('R/visualizer.R'); str(outline_process('vignettes/repro_example/analysis.R'), max.level = 2)"`
3. For the full UI, run:
   `devtools::load_all(".")`
   `serve_outline("vignettes/repro_example/analysis.R")`
4. For frontend work, use `inst/app`:
   - `npm install`
   - `npm run dev`
   - `npm run build`
   - `Copy-Item -Path dist\* -Destination ..\www -Recurse -Force`
5. `serve_outline()` is blocking. Stop it with `Esc` in an interactive R session.

## Current Direction
- The package has moved away from function-body string metadata as the primary UX.
- Comment annotations are now the preferred path.
- The frontend is organized around **steps** and **artifacts**, not just functions and objects.
- The intended output is both a developer-facing debugging tool and a cleaner publishable overview of analysis pipelines.
