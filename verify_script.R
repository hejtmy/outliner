# Verification Script
options(browser = function(url) message(paste("BROWSER OPEN:", url)))

# Source package functions
source("R/parser_meta.R")
source("R/parser_flow.R")
source("R/outline_parser.R")
source("R/visualizer.R")

# Source example functions so they are in the environment (for metadata extraction)
source("vignettes/repro_example/cleaning_functions.R")

# Run visualizer
message("Running outline_process...")
tryCatch(
    {
        serve_outline("vignettes/repro_example/analysis.R")
        message("VERIFICATION SUCCESSFUL")
    },
    error = function(e) {
        message("VERIFICATION FAILED: ", e$message)
    }
)
