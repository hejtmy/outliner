#' Parse Script Flow
#'
#' Analyses an R script or RMD/QMD file to find assignments and function calls,
#' tracking only specified objects if a header is present.
#'
#' @param file Path to the R script or RMD/QMD file.
#' @return A list of edges (from -> to) and nodes.
#' @export
parse_script_flow <- function(file) {
    # 1. Read file content
    lines <- readLines(file, warn = FALSE)
    content <- paste(lines, collapse = "\n")

    # 2. Extract Header (Tracked Objects)
    tracked_objects <- NULL

    # Check for Comment Header: # @outliner_track: obj1, obj2
    comment_header <- grep("^\\s*#\\s*@outliner_track\\s*:", lines, value = TRUE)
    if (length(comment_header) > 0) {
        # Take the first one found
        raw_list <- sub("^\\s*#\\s*@outliner_track\\s*:\\s*", "", comment_header[1])
        tracked_objects <- trimws(strsplit(raw_list, ",")[[1]])
    } else {
        # Check for YAML Header: outliner_track: [obj1, obj2] or similar
        # Simple regex for YAML line (assuming it's in the top frontmatter)
        # We look for "outliner_track:" in the first, say, 50 lines
        yaml_lines <- grep("^\\s*outliner_track\\s*:", lines[1:min(50, length(lines))], value = TRUE)
        if (length(yaml_lines) > 0) {
            # This is a bit rough, assuming inline list [a, b] or simple values
            # Improving regex to capture content
            raw_val <- sub("^\\s*outliner_track\\s*:\\s*", "", yaml_lines[1])
            # Clean up: keep only valid characters for R names (and comma/space)
            # This strips brackets, quotes, etc.
            raw_val <- gsub("[^a-zA-Z0-9_, .]", "", raw_val)
            tracked_objects <- trimws(strsplit(raw_val, ",")[[1]])
        }
    }

    # Clean tracked objects (remove empty strings)
    if (!is.null(tracked_objects)) {
        tracked_objects <- tracked_objects[tracked_objects != ""]
        if (length(tracked_objects) == 0) tracked_objects <- NULL
    }


    # 3. Handle File Type (R code extraction)
    ext <- tolower(tools::file_ext(file))
    code_text <- content

    if (ext %in% c("rmd", "qmd")) {
        # Simple extraction of R chunks: ```{r ...} ... ```
        # We will iterate lines to extract r chunks
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
        code_text <- paste(r_lines, collapse = "\n")
    }

    # 4. Parse Code
    # Use parse(text = ...)
    exprs <- tryCatch(parse(text = code_text), error = function(e) {
        warning("Failed to parse script: ", e$message)
        return(NULL)
    })

    edges <- list()
    nodes <- list()

    # Helper: Check if object should be tracked
    is_tracked <- function(name) {
        if (is.null(tracked_objects)) {
            return(TRUE)
        } # Track everything if no header
        return(name %in% tracked_objects)
    }

    # Helper to add node
    add_node <- function(name, type = "object") {
        # Filter if not tracked (only for data objects, function nodes are structural helpers but we filter them if they don't connect relevant objects?
        # Actually logic:
        # If type is object, must be in tracked list.
        # If type is function, we only add it if it connects tracked objects.
        # For simplicity: Add everything first, filter later? Or filter eagerly?
        # User requirement: "monitor only those objects ... It can ignore the rest of objects"

        if (type == "object" && !is_tracked(name)) {
            return(FALSE)
        }

        # Check if exists, else add
        if (!any(sapply(nodes, function(x) x$id == name))) {
            nodes <<- append(nodes, list(list(id = name, label = name, type = type)))
        }
        return(TRUE)
    }

    add_edge <- function(source, target, label = "") {
        # Only add edge if both nodes exist (meaning they were tracked)
        # Or at least one?
        # If we tracked 'data', and we have 'clean <- clean_data(data)',
        # 'clean' connects to 'clean_data' func, 'clean_data' func connects to 'data'.
        # If 'data' is tracked but 'clean' is NOT:
        # We see 'data' (tracked) -> func -> 'clean' (ignored).
        # We probably want to see the func consuming 'data'?
        # But if 'clean' is ignored, maybe the edge stops?
        # User said: "monitor only those objects AND anything that happens to them."
        # If 'clean' is assigned FROM 'data', 'clean' happens to 'data' (transformation).
        # BUT user also said "It can ignore the rest of objects".
        # Interpretation: Only show nodes for tracked objects. Showing the function call is fine IF it involves tracked objects.
        # Strict node filtering: Only add edges if Source AND Target nodes exist in our `nodes` list.

        # Check if source/target are in our nodes list
        source_exists <- any(sapply(nodes, function(x) x$id == source))
        target_exists <- any(sapply(nodes, function(x) x$id == target))

        # Exception: Functions are intermediate. We might need logic here.
        # If source is filtered object, source_exists is False.
        # If target is filtered object, target_exists is False.
        # If source OR target is valid, do we add?
        # If we only track 'A', and A -> func -> B.
        # A is valid. func is valid (helper). B is invalid.
        # Edge A -> func exists. Edge func -> B does not?
        # Then we have a dangling function.
        # Let's trust the add_node returns.

        if (source_exists && target_exists) {
            edges <<- append(edges, list(list(source = source, target = target, label = label)))
        }
    }

    # Recursive AST walker (simplified)
    walk_ast <- function(expr) {
        if (is.call(expr)) {
            op <- as.character(expr[[1]])

            # Assignment: target <- func(source)
            if (op %in% c("<-", "=")) {
                target <- as.character(expr[[2]])
                call_expr <- expr[[3]]

                # Check if target is tracked
                target_tracked <- add_node(target, "object")

                if (is.call(call_expr)) {
                    func_name <- as.character(call_expr[[1]])
                    if (length(func_name) > 1) func_name <- paste(func_name[2], func_name[3], sep = "::")

                    func_id <- paste0(func_name, "_", length(nodes))

                    # We only care about this function if it involves tracked objects
                    # But we don't know inputs yet.
                    # We'll optimistically add it, then maybe prune?
                    # Better: Check arguments first.

                    found_tracked_source <- FALSE
                    sources <- list()

                    # Inspect arguments
                    if (length(call_expr) >= 2) {
                        # naive: check all args
                        for (i in 2:length(call_expr)) {
                            arg <- call_expr[[i]]
                            if (is.symbol(arg)) {
                                source_name <- as.character(arg)
                                if (is_tracked(source_name)) {
                                    found_tracked_source <- TRUE
                                    sources <- c(sources, source_name)
                                }
                            }
                        }
                    }

                    # If target is tracked OR we found a tracked source, we should show this flow?
                    # If target is NOT tracked, but source IS: e.g. ignored <- func(tracked).
                    # 'tracked' is being used. We might want to see that?
                    # User: "monitor only those objects"
                    # If 'ignored' is not tracked, we probably don't care where 'tracked' goes unless it goes into another 'tracked'.
                    # "Every assignment and reassignment" [of the tracked objects].
                    # So if tracked -> tracked, show it.
                    # If tracked -> ignored, maybe ignore it?
                    # Let's stick to strict: only show if Object is in tracked list.

                    if (target_tracked || found_tracked_source) {
                        # We need the function node
                        add_node(func_id, "function")
                        nodes[[length(nodes)]]$func_name <<- func_name

                        # Add edges from sources
                        for (src in sources) {
                            add_node(src, "object") # Ensure it's added
                            add_edge(src, func_id)
                        }

                        # Add edge to target
                        if (target_tracked) {
                            add_edge(func_id, target)
                        }
                    }
                }
            }

            # Handle Native Pipe |>
            if (op == "|>") {
                lhs <- expr[[2]]
                rhs <- expr[[3]]

                # Resolving LHS source
                lhs_name <- NULL
                if (is.symbol(lhs)) {
                    lhs_name <- as.character(lhs)
                }

                # Resolve RHS function
                if (is.call(rhs)) {
                    func_name <- as.character(rhs[[1]])
                    if (length(func_name) > 1) func_name <- paste(func_name[2], func_name[3], sep = "::")

                    func_id <- paste0(func_name, "_", length(nodes))

                    # Check tracking
                    source_tracked <- FALSE
                    if (!is.null(lhs_name)) {
                        source_tracked <- is_tracked(lhs_name)
                    }

                    # Note: We are inside a pipe. If this pipe is part of an assignment,
                    # we don't know the target here easily in this recursion without passing down context.
                    # Current parser limit: It handles top-level pipes well, but nested pipes inside assignment
                    # are tricky because 'walk_ast' is called on the assignment RHS already.
                    # But wait, if this |> is inside an assignment, the Assignment block above would have handled `call_expr` which is the pipe!
                    # `<-` calls walk_ast(call_expr). `call_expr` is `|>`.
                    # So we are here.
                    # But we don't know the `target` from the parent scope.

                    # Determine if we should record this function node
                    # If existing source is tracked, we strictly add the function.
                    if (source_tracked) {
                        add_node(lhs_name, "object")
                        add_node(func_id, "function")
                        nodes[[length(nodes)]]$func_name <<- func_name
                        add_edge(lhs_name, func_id)

                        # Note: The edge from func_id to Target effectively gets lost here
                        # because we don't know Target.
                        # Improvements to parser logic needed to fully support pipes in assignment + tracking?
                        # For now, we preserve existing behavior but filtered.
                    }
                }
            }
        }
    }

    if (!is.null(exprs)) {
        for (i in seq_along(exprs)) {
            walk_ast(exprs[[i]])
        }
    }

    list(nodes = nodes, edges = edges)
}
