#' Generate Outline Data
#'
#' Orchestrates the parsing of a script and extraction of metadata to produce
#' the JSON structure required by the XYFlow frontend.
#'
#' @param script_path Path to the R script or Rmd/Qmd file.
#' @param output_path Optional path to save the JSON.
#' @return A list containing the graph data (nodes and edges).
#' @export
outline_process <- function(script_path, output_path = NULL) {
    # 1. Parse Flow
    flow <- parse_script_flow(script_path)

    # 2. Enrich Nodes with Metadata

    # create a sandbox environment to load definitions
    sandbox <- new.env(parent = baseenv())

    # Helper to source files found in the script
    # We need to re-parse the script specifically looking for source() calls
    # or use the AST from parse_script_flow if we returned it (we didn't, we returned nodes/edges)

    # Quick parse for source calls
    # Handle RMD/QMD by extracting code first
    ext <- tolower(tools::file_ext(script_path))
    temp_r_file <- NULL

    if (ext %in% c("rmd", "qmd")) {
        lines <- readLines(script_path, warn = FALSE)
        r_lines <- c()
        in_chunk <- FALSE
        for (line in lines) {
            if (grepl("^\\s*```+\\s*\\{r", line, ignore.case = TRUE)) {
                in_chunk <- TRUE
                next
            }
            if (in_chunk && grepl("^\\s*```+", line)) {
                in_chunk <- FALSE
                next
            }
            if (in_chunk) {
                r_lines <- c(r_lines, line)
            }
        }
        # Write to temp file to allow parsing and sourcing
        temp_r_file <- tempfile(fileext = ".R")
        writeLines(r_lines, temp_r_file)

        # Use simple parse on the temp file
        parsed <- parse(temp_r_file)

        # Use temp file path for sourcing logic, but we need to resolve relative paths from the ORIGINAL file directory.
        # This is tricky because sys.source might expect CWD
        # The 'base_dir' below handles relative resolution.
    } else {
        parsed <- parse(script_path)
    }

    base_dir <- dirname(script_path)

    # Function to walk AST and find source()
    find_sources <- function(expr) {
        if (is.call(expr)) {
            if (as.character(expr[[1]]) == "source") {
                # source("file.R")
                if (length(expr) >= 2 && is.character(expr[[2]])) {
                    fpath <- expr[[2]]
                    # resolve relative path
                    full_path <- file.path(base_dir, fpath)
                    if (file.exists(full_path)) {
                        tryCatch(sys.source(full_path, envir = sandbox), error = function(e) warning(paste("Failed to source", full_path)))
                    } else {
                        # Try raw path just in case
                        if (file.exists(fpath)) {
                            tryCatch(sys.source(fpath, envir = sandbox), error = function(e) warning(paste("Failed to source", fpath)))
                        }
                    }
                }
            } else {
                lapply(expr, find_sources)
            }
        }
    }
    for (i in seq_along(parsed)) find_sources(parsed[[i]])

    # Also check global env as fallback
    # We unite sandbox and global lookup
    get_fun <- function(name) {
        if (exists(name, envir = sandbox, inherits = FALSE)) {
            return(get(name, envir = sandbox))
        }
        if (exists(name, envir = .GlobalEnv)) {
            return(get(name, envir = .GlobalEnv))
        }
        NULL
    }

    for (i in seq_along(flow$nodes)) {
        node <- flow$nodes[[i]]
        if (node$type == "function" && !is.null(node$func_name)) {
            # Try to get the function object
            fun_obj <- tryCatch(get_fun(node$func_name), error = function(e) NULL)

            if (is.function(fun_obj)) {
                meta <- extract_meta(fun_obj)
                if (!is.null(meta)) {
                    flow$nodes[[i]]$data <- list(
                        label = meta$name, # Use human name
                        short = meta$short,
                        long = meta$long
                    )
                } else {
                    flow$nodes[[i]]$data <- list(label = node$func_name)
                }
            } else {
                flow$nodes[[i]]$data <- list(label = node$func_name)
            }
        } else {
            if (is.null(node$data)) flow$nodes[[i]]$data <- list(label = node$label)
        }

        # Add XYFlow specific positioning (dummy for now, frontend will layout)
        flow$nodes[[i]]$position <- list(x = 0, y = 0)
    }

    graph_data <- list(nodes = flow$nodes, edges = flow$edges)

    if (!is.null(output_path)) {
        jsonlite::write_json(graph_data, output_path, auto_unbox = TRUE)
    }

    return(graph_data)
}

#' Serve the Outline Visualizer
#'
#' Starts a local web server to display the visualization.
#' This generates a temporary build of the app with the data injected,
#' and serves it using httpuv to avoid CORS issues with ES modules.
#'
#' @param script_path Path to the script to visualize.
#' @param port Port to run the server on (default: random).
#' @param launch_browser Logical, whether to open the browser automatically.
#' @export
serve_outline <- function(script_path, port = 0, launch_browser = TRUE) {
    # Generate data
    data <- outline_process(script_path)
    json_data <- jsonlite::toJSON(data, auto_unbox = TRUE)

    # Locate the built React app
    # In a package, this would be system.file("www", package = "outliner")
    # For dev, we point to inst/www or inst/app/dist

    app_dir <- system.file("www", package = "outliner")
    if (app_dir == "") {
        # Fallback for dev mode
        # Check for where we built the app
        if (dir.exists("inst/app/dist")) {
            app_dir <- "inst/app/dist"
        } else if (dir.exists("inst/www") && length(list.files("inst/www")) > 0) {
            app_dir <- "inst/www"
        } else {
            # Absolute path fallback
            if (dir.exists("d:/Projects/R/outliner/inst/app/dist")) {
                app_dir <- "d:/Projects/R/outliner/inst/app/dist"
            } else {
                stop("Could not find the React app. Did you build it?")
            }
        }
    }

    # Create a staging directory
    stage_dir <- file.path(tempdir(), paste0("outliner_", format(Sys.time(), "%H%M%S")))
    if (dir.exists(stage_dir)) unlink(stage_dir, recursive = TRUE)
    dir.create(stage_dir)

    # Copy app files
    files <- list.files(app_dir, full.names = TRUE)
    if (length(files) > 0) {
        file.copy(files, stage_dir, recursive = TRUE)
    }

    # Inject data into index.html
    index_file <- file.path(stage_dir, "index.html")
    if (!file.exists(index_file)) {
        stop(paste("index.html not found in", stage_dir))
    }

    html_content <- readLines(index_file, warn = FALSE)
    html_content <- paste(html_content, collapse = "\n")

    injection <- sprintf("<script>window.outlinerData = %s;</script>", json_data)

    if (grepl("</head>", html_content)) {
        new_html <- sub("</head>", paste0(injection, "\n</head>"), html_content)
    } else {
        new_html <- paste(injection, html_content)
    }

    writeLines(new_html, index_file)

    # Server logic
    app <- list(
        call = function(req) {
            list(
                status = 200,
                body = "OK",
                headers = list(
                    "Content-Type" = "text/plain"
                )
            )
        },
        staticPaths = list(
            "/" = stage_dir
        )
    )

    # Use 0.0.0.0 to bind to all interfaces (more robust on Windows)
    host <- "0.0.0.0"

    message("Starting server...")
    server_id <- httpuv::startServer(host, port, app)

    on.exit({
        httpuv::stopServer(server_id)
        message("Server stopped.")
    })

    actual_port <- server_id$getPort()
    # construct URL for user (use loopback 127.0.0.1 for browser)
    url <- paste0("http://127.0.0.1:", actual_port)
    message(paste("Serving outliner at", url, "(Press Esc to stop)"))

    if (launch_browser) {
        tryCatch(
            {
                utils::browseURL(url)
            },
            error = function(e) {
                message("Could not open browser automatically: ", e$message)
                message("Please open the URL manually.")
            }
        )
    }

    # Block to keep serving
    while (TRUE) {
        httpuv::service()
        Sys.sleep(0.001)
    }
}
