#' Parse Script Outline
#'
#' Builds an annotation-first graph of analysis steps and data artifacts without
#' executing user code. The parser understands lightweight `# @outliner_step`
#' comment blocks and uses static analysis as fallback when annotations are
#' missing.
#'
#' @param file Path to the R script or RMD/QMD file.
#' @return A list with nodes, edges, and metadata for the frontend.
#' @export
parse_script_outline <- function(file) {
    context <- build_outline_context(file)
    build_outline_graph(context)
}

build_outline_context <- function(root_file) {
    root <- normalizePath(root_file, winslash = "/", mustWork = TRUE)
    queue <- root
    seen <- character()
    documents <- list()
    function_index <- list()

    while (length(queue) > 0) {
        current <- queue[[1]]
        queue <- queue[-1]

        if (current %in% seen) {
            next
        }

        seen <- c(seen, current)
        doc <- read_outline_document(current)
        doc_info <- parse_outline_document(doc)
        documents[[current]] <- doc_info

        defs <- collect_function_definitions(doc_info)
        for (name in names(defs)) {
            function_index[[name]] <- defs[[name]]
        }

        if (length(doc_info$source_files) > 0) {
            queue <- c(queue, setdiff(doc_info$source_files, seen))
        }
    }

    list(
        root = root,
        documents = documents,
        function_index = function_index
    )
}

read_outline_document <- function(file) {
    lines <- readLines(file, warn = FALSE)
    ext <- tolower(tools::file_ext(file))
    tracked_objects <- extract_tracked_objects(lines)

    if (ext %in% c("rmd", "qmd")) {
        extracted <- extract_r_chunks(lines)
    } else {
        extracted <- list(
            code_lines = lines,
            line_map = seq_along(lines),
            chunk_map = rep(NA_character_, length(lines))
        )
    }

    list(
        file = normalizePath(file, winslash = "/", mustWork = TRUE),
        type = ext,
        title = basename(file),
        lines = lines,
        code_lines = extracted$code_lines,
        code_text = paste(extracted$code_lines, collapse = "\n"),
        line_map = extracted$line_map,
        chunk_map = extracted$chunk_map,
        tracked_objects = tracked_objects
    )
}

extract_tracked_objects <- function(lines) {
    tracked_objects <- NULL
    comment_header <- grep("^\\s*#\\s*@outliner_track\\s*:", lines, value = TRUE)

    if (length(comment_header) > 0) {
        raw_list <- sub("^\\s*#\\s*@outliner_track\\s*:\\s*", "", comment_header[1])
        tracked_objects <- trimws(strsplit(raw_list, ",", fixed = TRUE)[[1]])
    } else {
        yaml_window <- lines[seq_len(min(50, length(lines)))]
        yaml_lines <- grep("^\\s*outliner_track\\s*:", yaml_window, value = TRUE)
        if (length(yaml_lines) > 0) {
            raw_val <- sub("^\\s*outliner_track\\s*:\\s*", "", yaml_lines[1])
            raw_val <- gsub("[^a-zA-Z0-9_, .]", "", raw_val)
            tracked_objects <- trimws(strsplit(raw_val, ",", fixed = TRUE)[[1]])
        }
    }

    tracked_objects <- tracked_objects[nzchar(tracked_objects)]
    if (length(tracked_objects) == 0) {
        return(NULL)
    }

    unique(tracked_objects)
}

extract_r_chunks <- function(lines) {
    code_lines <- character()
    line_map <- integer()
    chunk_map <- character()
    in_chunk <- FALSE
    chunk_label <- NA_character_
    chunk_index <- 0

    for (i in seq_along(lines)) {
        line <- lines[i]

        if (!in_chunk && grepl("^\\s*```+\\s*\\{r", line, ignore.case = TRUE)) {
            in_chunk <- TRUE
            chunk_index <- chunk_index + 1
            chunk_label <- parse_chunk_label(line, chunk_index)
            next
        }

        if (in_chunk && grepl("^\\s*```+", line)) {
            in_chunk <- FALSE
            chunk_label <- NA_character_
            next
        }

        if (in_chunk) {
            code_lines <- c(code_lines, line)
            line_map <- c(line_map, i)
            chunk_map <- c(chunk_map, chunk_label)
        }
    }

    list(
        code_lines = code_lines,
        line_map = line_map,
        chunk_map = chunk_map
    )
}

parse_chunk_label <- function(line, chunk_index) {
    chunk_text <- sub("^\\s*```+\\s*\\{", "", line)
    chunk_text <- sub("\\}\\s*$", "", chunk_text)
    chunk_text <- trimws(sub("^[rR]\\s*", "", chunk_text))

    if (!nzchar(chunk_text)) {
        return(paste0("chunk-", chunk_index))
    }

    first <- trimws(strsplit(chunk_text, ",", fixed = TRUE)[[1]][1])
    if (grepl("=", first, fixed = TRUE) || !nzchar(first)) {
        return(paste0("chunk-", chunk_index))
    }

    first
}

parse_outline_document <- function(document) {
    annotation_blocks <- collect_annotation_blocks(
        document$code_lines,
        document$line_map,
        document$chunk_map
    )

    exprs <- NULL
    if (nzchar(document$code_text)) {
        exprs <- tryCatch(
            parse(text = document$code_text, keep.source = TRUE),
            error = function(e) {
                warning("Failed to parse script: ", e$message)
                NULL
            }
        )
    }

    expressions <- list()
    source_files <- character()
    source_refs <- if (!is.null(exprs)) attr(exprs, "srcref") else NULL

    if (!is.null(exprs)) {
        for (i in seq_along(exprs)) {
            expr <- exprs[[i]]
            source_ref <- if (!is.null(source_refs) && length(source_refs) >= i) source_refs[[i]] else NULL
            if (is.null(source_ref)) {
                next
            }

            range <- srcref_to_range(source_ref, document)
            annotation <- find_attached_annotation(annotation_blocks, range$code_start)
            expressions[[length(expressions) + 1]] <- list(
                expr = expr,
                source = range,
                annotation = annotation,
                code = extract_original_code(document$lines, range$line_start, range$line_end)
            )
            source_files <- c(source_files, find_source_calls(expr, dirname(document$file)))
        }
    }

    list(
        document = document,
        annotation_blocks = annotation_blocks,
        expressions = expressions,
        source_files = unique(source_files)
    )
}

srcref_to_range <- function(source_ref, document) {
    code_start <- as.integer(source_ref[1])
    code_end <- as.integer(source_ref[3])
    line_start <- document$line_map[code_start]
    line_end <- document$line_map[code_end]

    list(
        code_start = code_start,
        code_end = code_end,
        line_start = line_start,
        line_end = line_end,
        chunk = if (length(document$chunk_map) >= code_start) document$chunk_map[code_start] else NA_character_,
        file = document$file
    )
}

find_attached_annotation <- function(blocks, code_start) {
    if (length(blocks) == 0) {
        return(NULL)
    }

    for (block in blocks) {
        if (!is.na(block$attached_line) && identical(block$attached_line, code_start)) {
            return(block)
        }
    }

    NULL
}

extract_original_code <- function(lines, start_line, end_line) {
    if (length(lines) == 0 || is.na(start_line) || is.na(end_line)) {
        return("")
    }

    paste(lines[start_line:end_line], collapse = "\n")
}

find_source_calls <- function(expr, base_dir) {
    matches <- character()

    walk <- function(node) {
        if (!is.call(node)) {
            return(NULL)
        }

        call_name <- call_head_name(node)
        if (identical(call_name, "source") && length(node) >= 2 && is.character(node[[2]])) {
            target <- node[[2]]
            candidate <- file.path(base_dir, target)
            if (file.exists(candidate)) {
                matches <<- c(matches, normalizePath(candidate, winslash = "/", mustWork = TRUE))
            } else if (file.exists(target)) {
                matches <<- c(matches, normalizePath(target, winslash = "/", mustWork = TRUE))
            }
        }

        for (i in seq_along(node)) {
            walk(node[[i]])
        }
    }

    walk(expr)
    unique(matches)
}

collect_function_definitions <- function(doc_info) {
    defs <- list()

    for (expr_info in doc_info$expressions) {
        expr <- expr_info$expr
        if (!is_function_definition(expr)) {
            next
        }

        fun_name <- assignment_target_name(expr[[2]])
        if (is.null(fun_name)) {
            next
        }

        fun_expr <- expr[[3]]
        fun_obj <- tryCatch(eval(fun_expr, envir = baseenv()), error = function(e) NULL)
        params <- if (is.function(fun_obj)) names(formals(fun_obj)) else character()
        legacy_meta <- if (is.function(fun_obj)) extract_meta(fun_obj) else NULL

        function_annotation <- merge_function_annotation(expr_info$annotation, legacy_meta)
        internal_steps <- extract_internal_steps(doc_info, expr_info$source)

        defs[[fun_name]] <- list(
            name = fun_name,
            params = params,
            file = doc_info$document$file,
            source = expr_info$source,
            annotation = function_annotation,
            internal_steps = internal_steps,
            code = expr_info$code
        )
    }

    defs
}

merge_function_annotation <- function(annotation, legacy_meta = NULL) {
    merged <- if (is.null(annotation)) empty_annotation() else annotation

    if (!is.null(legacy_meta)) {
        if (is.null(merged$label) && !is.null(legacy_meta$name)) {
            merged$label <- legacy_meta$name
        }
        if (is.null(merged$summary) && !is.null(legacy_meta$short)) {
            merged$summary <- legacy_meta$short
        }
        if (length(merged$details) == 0 && !is.null(legacy_meta$long)) {
            merged$details <- legacy_meta$long
        }
    }

    merged
}

extract_internal_steps <- function(doc_info, source_range) {
    blocks <- Filter(
        function(block) {
            block$code_line_start >= source_range$code_start &&
                block$code_line_end <= source_range$code_end
        },
        doc_info$annotation_blocks
    )

    if (length(blocks) == 0) {
        return(list())
    }

    steps <- list()
    for (i in seq_along(blocks)) {
        block <- blocks[[i]]
        code_start <- find_next_code_line(doc_info$document$code_lines, block$code_line_end + 1)
        next_block_start <- if (i < length(blocks)) blocks[[i + 1]]$code_line_start - 1 else source_range$code_end
        code_end <- min(next_block_start, source_range$code_end)

        steps[[length(steps) + 1]] <- list(
            label = block$label,
            summary = block$summary,
            details = block$details,
            inputs = block$inputs,
            outputs = block$outputs,
            modifies = block$modifies,
            tags = block$tags,
            section = block$section,
            publish = block$publish,
            code = extract_code_slice(doc_info$document$code_lines, code_start, code_end),
            source = map_code_slice_to_source(doc_info$document, code_start, code_end, block$chunk)
        )
    }

    steps
}

extract_code_slice <- function(code_lines, start_index, end_index) {
    if (is.na(start_index) || is.na(end_index) || start_index > end_index) {
        return("")
    }

    lines <- trim_code_lines(code_lines[start_index:end_index])
    paste(lines, collapse = "\n")
}

trim_code_lines <- function(lines) {
    if (length(lines) == 0) {
        return(lines)
    }

    keep <- seq_along(lines)
    while (length(keep) > 0 && trimws(lines[keep[1]]) %in% c("", "{")) {
        keep <- keep[-1]
    }
    while (length(keep) > 0 && trimws(lines[keep[length(keep)]]) %in% c("", "}")) {
        keep <- keep[-length(keep)]
    }

    if (length(keep) == 0) {
        return(character())
    }

    lines[keep]
}

map_code_slice_to_source <- function(document, start_index, end_index, chunk = NA_character_) {
    if (is.na(start_index) || is.na(end_index) || start_index > end_index) {
        return(NULL)
    }

    list(
        file = document$file,
        line_start = document$line_map[start_index],
        line_end = document$line_map[end_index],
        chunk = chunk
    )
}

build_outline_graph <- function(context) {
    root_doc <- context$documents[[context$root]]
    nodes <- list()
    node_lookup <- new.env(parent = emptyenv())
    edges <- list()
    edge_lookup <- new.env(parent = emptyenv())
    step_counter <- 0L
    edge_counter <- 0L

    add_node <- function(node) {
        if (exists(node$id, envir = node_lookup, inherits = FALSE)) {
            return(node$id)
        }
        nodes[[length(nodes) + 1]] <<- node
        assign(node$id, length(nodes), envir = node_lookup)
        node$id
    }

    add_artifact <- function(name) {
        clean_name <- trimws(name)
        if (!nzchar(clean_name)) {
            return(NULL)
        }

        artifact_id <- artifact_node_id(clean_name)
        add_node(list(
            id = artifact_id,
            type = "artifact",
            data = list(
                label = clean_name,
                name = clean_name,
                kind = "artifact"
            )
        ))
        artifact_id
    }

    add_edge <- function(source, target, kind) {
        if (is.null(source) || is.null(target)) {
            return(NULL)
        }

        key <- paste(source, target, kind, sep = "||")
        if (exists(key, envir = edge_lookup, inherits = FALSE)) {
            return(NULL)
        }

        edge_counter <<- edge_counter + 1L
        edges[[length(edges) + 1]] <<- list(
            id = paste0("edge:", edge_counter),
            source = source,
            target = target,
            data = list(kind = kind)
        )
        assign(key, TRUE, envir = edge_lookup)
    }

    add_step_sequence <- function(specs) {
        previous_step <- NULL
        for (spec in specs) {
            step_counter <<- step_counter + 1L
            step_id <- paste0("step:", step_counter)

            add_node(list(
                id = step_id,
                type = "step",
                data = spec
            ))

            for (input in unique(spec$inputs)) {
                artifact_id <- add_artifact(input)
                add_edge(artifact_id, step_id, "consumes")
            }

            if (!is.null(previous_step)) {
                add_edge(previous_step, step_id, "sequence")
            }

            for (output in unique(spec$outputs)) {
                artifact_id <- add_artifact(output)
                add_edge(step_id, artifact_id, "produces")
            }

            previous_step <- step_id
        }
    }

    for (expr_info in root_doc$expressions) {
        step_specs <- expression_to_step_specs(expr_info, context$function_index)
        if (length(step_specs) > 0) {
            add_step_sequence(step_specs)
        }
    }

    list(
        nodes = nodes,
        edges = edges,
        meta = list(
            title = basename(context$root),
            root_file = context$root,
            tracked_objects = root_doc$document$tracked_objects,
            documents = lapply(context$documents, function(doc_info) {
                list(
                    file = doc_info$document$file,
                    title = doc_info$document$title,
                    type = doc_info$document$type
                )
            })
        )
    )
}

expression_to_step_specs <- function(expr_info, function_index) {
    expr <- expr_info$expr

    if (is_function_definition(expr) || is_source_call(expr)) {
        return(list())
    }

    annotation <- expr_info$annotation
    target <- if (is_assignment(expr)) assignment_target_name(expr[[2]]) else NULL
    rhs <- if (is_assignment(expr)) expr[[3]] else expr
    inferred_inputs <- unique(setdiff(collect_symbols(rhs), target))

    if (!is.null(annotation)) {
        return(list(make_annotation_step_spec(annotation, expr_info, inferred_inputs, target, rhs, function_index)))
    }

    if (is.call(rhs) && identical(call_head_name(rhs), "|>")) {
        return(build_pipe_step_specs(rhs, expr_info, function_index, target))
    }

    if (is.call(rhs)) {
        call_name <- call_head_name(rhs)
        definition <- function_index[[call_name]]

        if (!is.null(definition) && length(definition$internal_steps) > 0) {
            return(expand_definition_steps(definition, rhs, expr_info, inferred_inputs, target))
        }

        return(list(make_call_step_spec(rhs, definition, expr_info, inferred_inputs, target)))
    }

    if (!is.null(target) || length(inferred_inputs) > 0) {
        return(list(make_expression_step_spec(expr_info, inferred_inputs, target)))
    }

    list()
}

make_annotation_step_spec <- function(annotation, expr_info, inferred_inputs, target, rhs, function_index) {
    call_name <- if (is.call(rhs)) call_head_name(rhs) else NULL
    definition <- if (!is.null(call_name)) function_index[[call_name]] else NULL

    list(
        label = pick_first_text(annotation$label, if (!is.null(definition)) definition$annotation$label, default = fallback_step_label(target, call_name)),
        summary = pick_first_text(annotation$summary, if (!is.null(definition)) definition$annotation$summary),
        details = normalize_details(annotation$details),
        inputs = pick_first_vector(annotation$inputs, inferred_inputs),
        outputs = pick_first_vector(annotation$outputs, target),
        modifies = annotation$modifies,
        tags = annotation$tags,
        section = annotation$section,
        publish = if (!is.null(annotation$publish)) annotation$publish else TRUE,
        kind = "annotated_step",
        function_name = call_name,
        code = expr_info$code,
        source = expr_info$source,
        definition_source = if (!is.null(definition)) definition$source else NULL,
        call_source = expr_info$source
    )
}

make_call_step_spec <- function(call_expr, definition, expr_info, inferred_inputs, target) {
    call_name <- call_head_name(call_expr)

    list(
        label = pick_first_text(if (!is.null(definition)) definition$annotation$label, default = humanize_name(call_name)),
        summary = pick_first_text(if (!is.null(definition)) definition$annotation$summary),
        details = normalize_details(if (!is.null(definition)) definition$annotation$details else NULL),
        inputs = inferred_inputs,
        outputs = pick_first_vector(target),
        modifies = character(),
        tags = if (!is.null(definition)) definition$annotation$tags else character(),
        section = if (!is.null(definition)) definition$annotation$section else NULL,
        publish = TRUE,
        kind = "function_call",
        function_name = call_name,
        code = expr_info$code,
        source = expr_info$source,
        definition_source = if (!is.null(definition)) definition$source else NULL,
        call_source = expr_info$source
    )
}

make_expression_step_spec <- function(expr_info, inferred_inputs, target) {
    list(
        label = fallback_step_label(target, NULL),
        summary = NULL,
        details = character(),
        inputs = inferred_inputs,
        outputs = pick_first_vector(target),
        modifies = character(),
        tags = character(),
        section = NULL,
        publish = TRUE,
        kind = "expression",
        function_name = NULL,
        code = expr_info$code,
        source = expr_info$source,
        definition_source = NULL,
        call_source = expr_info$source
    )
}

build_pipe_step_specs <- function(pipe_expr, expr_info, function_index, target) {
    parts <- decompose_pipe(pipe_expr)
    stages <- parts$stages
    source_inputs <- collect_symbols(parts$source)
    specs <- list()

    for (i in seq_along(stages)) {
        stage <- stages[[i]]
        stage_name <- call_head_name(stage)
        definition <- function_index[[stage_name]]
        stage_inputs <- unique(c(if (i == 1) source_inputs else character(), collect_call_argument_symbols(stage)))
        stage_target <- if (i == length(stages)) target else NULL

        if (!is.null(definition) && length(definition$internal_steps) > 0) {
            specs <- c(specs, expand_definition_steps(definition, stage, expr_info, stage_inputs, stage_target))
        } else {
            specs[[length(specs) + 1]] <- make_call_step_spec(stage, definition, expr_info, stage_inputs, stage_target)
        }
    }

    specs
}

expand_definition_steps <- function(definition, call_expr, expr_info, inferred_inputs, target) {
    arg_map <- map_call_arguments(definition$params, call_expr)
    steps <- definition$internal_steps
    specs <- list()

    for (i in seq_along(steps)) {
        step <- steps[[i]]
        step_inputs <- substitute_names(step$inputs, arg_map)
        step_outputs <- substitute_names(step$outputs, arg_map)
        step_modifies <- substitute_names(step$modifies, arg_map)

        if (i == 1 && length(step_inputs) == 0) {
            step_inputs <- inferred_inputs
        } else if (i > 1) {
            step_inputs <- character()
        }

        if (i == length(steps) && length(step_outputs) == 0 && !is.null(target)) {
            step_outputs <- target
        } else if (i < length(steps)) {
            step_outputs <- character()
        }

        specs[[length(specs) + 1]] <- list(
            label = pick_first_text(step$label, definition$annotation$label, default = humanize_name(definition$name)),
            summary = pick_first_text(step$summary, definition$annotation$summary),
            details = normalize_details(step$details),
            inputs = unique(step_inputs),
            outputs = unique(step_outputs),
            modifies = unique(step_modifies),
            tags = unique(c(step$tags, definition$annotation$tags)),
            section = pick_first_text(step$section, definition$annotation$section),
            publish = if (!is.null(step$publish)) step$publish else TRUE,
            kind = "annotated_function_step",
            function_name = definition$name,
            code = step$code,
            source = step$source,
            definition_source = definition$source,
            call_source = expr_info$source,
            invocation_code = expr_info$code
        )
    }

    specs
}

map_call_arguments <- function(params, call_expr) {
    mapping <- list()
    args <- as.list(call_expr)[-1]
    arg_names <- names(args)
    positional_index <- 1L

    for (i in seq_along(args)) {
        arg <- args[[i]]
        param_name <- NULL

        if (!is.null(arg_names) && nzchar(arg_names[i])) {
            param_name <- arg_names[i]
        } else if (length(params) >= positional_index) {
            param_name <- params[positional_index]
            positional_index <- positional_index + 1L
        }

        if (!is.null(param_name)) {
            mapping[[param_name]] <- expr_to_string(arg)
        }
    }

    mapping
}

substitute_names <- function(values, mapping) {
    if (length(values) == 0) {
        return(character())
    }

    vapply(values, function(value) {
        if (!is.null(mapping[[value]])) mapping[[value]] else value
    }, character(1))
}

decompose_pipe <- function(expr) {
    if (!is.call(expr) || !identical(call_head_name(expr), "|>")) {
        return(list(source = expr, stages = list()))
    }

    left <- decompose_pipe(expr[[2]])
    list(
        source = left$source,
        stages = c(left$stages, list(expr[[3]]))
    )
}

collect_symbols <- function(expr) {
    if (is.null(expr)) {
        return(character())
    }

    if (is.symbol(expr)) {
        return(as.character(expr))
    }

    if (!is.call(expr)) {
        return(character())
    }

    op <- call_head_name(expr)
    if (op %in% c("<-", "=")) {
        return(collect_symbols(expr[[3]]))
    }
    if (op %in% c("$", "@", "[", "[[")) {
        return(collect_symbols(expr[[2]]))
    }
    if (op == "~") {
        return(character())
    }

    values <- character()
    for (i in seq.int(2, length(expr))) {
        values <- c(values, collect_symbols(expr[[i]]))
    }

    unique(values)
}

collect_call_argument_symbols <- function(call_expr) {
    if (!is.call(call_expr)) {
        return(character())
    }

    values <- character()
    for (i in seq.int(2, length(call_expr))) {
        values <- c(values, collect_symbols(call_expr[[i]]))
    }
    unique(values)
}

is_assignment <- function(expr) {
    is.call(expr) && call_head_name(expr) %in% c("<-", "=")
}

is_source_call <- function(expr) {
    is.call(expr) && identical(call_head_name(expr), "source")
}

is_function_definition <- function(expr) {
    is_assignment(expr) &&
        is.call(expr[[3]]) &&
        identical(as.character(expr[[3]][[1]]), "function")
}

assignment_target_name <- function(lhs) {
    if (is.symbol(lhs)) {
        return(as.character(lhs))
    }

    if (is.call(lhs) && call_head_name(lhs) %in% c("$", "@", "[", "[[")) {
        return(assignment_target_name(lhs[[2]]))
    }

    NULL
}

call_head_name <- function(expr) {
    if (is.null(expr)) {
        return(NULL)
    }

    if (is.symbol(expr)) {
        return(as.character(expr))
    }

    if (!is.call(expr)) {
        return(NULL)
    }

    head <- expr[[1]]
    if (is.symbol(head)) {
        return(as.character(head))
    }
    if (is.call(head) && identical(as.character(head[[1]]), "::")) {
        return(paste(as.character(head[[2]]), as.character(head[[3]]), sep = "::"))
    }

    expr_to_string(head)
}

expr_to_string <- function(expr) {
    paste(deparse(expr, width.cutoff = 500L), collapse = " ")
}

artifact_node_id <- function(name) {
    paste0("artifact:", gsub("[^A-Za-z0-9_.:-]", "_", name))
}

pick_first_text <- function(..., default = NULL) {
    values <- list(...)
    for (value in values) {
        if (is.null(value) || length(value) == 0) {
            next
        }
        text <- trimws(as.character(value[[1]]))
        if (nzchar(text)) {
            return(text)
        }
    }
    default
}

pick_first_vector <- function(..., default = character()) {
    values <- list(...)
    for (value in values) {
        if (is.null(value) || length(value) == 0) {
            next
        }
        cleaned <- unique(trimws(as.character(value)))
        cleaned <- cleaned[nzchar(cleaned)]
        if (length(cleaned) > 0) {
            return(cleaned)
        }
    }
    default
}

normalize_details <- function(details) {
    if (is.null(details) || length(details) == 0) {
        return(character())
    }
    details <- as.character(details)
    details[nzchar(details) | details == ""]
}

humanize_name <- function(name) {
    label <- gsub("([a-z0-9])([A-Z])", "\\1 \\2", name)
    label <- gsub("[._]+", " ", label)
    label <- trimws(label)
    if (!nzchar(label)) {
        return("Step")
    }

    words <- strsplit(label, "\\s+")[[1]]
    paste(toupper(substring(words, 1, 1)), substring(words, 2), collapse = " ")
}

fallback_step_label <- function(target, function_name) {
    if (!is.null(function_name)) {
        return(humanize_name(function_name))
    }
    if (!is.null(target)) {
        return(paste("Update", target))
    }
    "Analysis Step"
}
