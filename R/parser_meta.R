#' Extract Metadata from Function
#'
#' Parses a legacy string literal from the first expression in a function body.
#' This remains supported for backwards compatibility, but the primary
#' annotation path is now comment-based and handled statically.
#'
#' @param fun A function object.
#' @return A named list with metadata keys (e.g., name, short, long), or NULL if not found.
#' @export
extract_meta <- function(fun) {
    if (!is.function(fun)) {
        return(NULL)
    }

    b <- body(fun)
    first_expr <- NULL
    if (as.character(b[[1]]) == "{") {
        if (length(b) >= 2) {
            first_expr <- b[[2]]
        }
    } else {
        first_expr <- b
    }

    if (is.character(first_expr)) {
        return(parse_meta_string(first_expr))
    }

    NULL
}

#' Parse Metadata String
#'
#' @param text The string containing YAML-like metadata.
#' @return A named list.
#' @keywords internal
parse_meta_string <- function(text) {
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
    meta <- list()
    current_key <- NULL
    current_val <- character()

    for (line in lines) {
        trim <- trimws(line)
        if (trim == "") {
            next
        }

        if (grepl("^:[a-zA-Z]+:", trim)) {
            if (!is.null(current_key)) {
                meta[[current_key]] <- trimws(paste(current_val, collapse = " "))
            }

            matches <- regexpr("^:[a-zA-Z]+:", trim)
            key_part <- regmatches(trim, matches)
            current_key <- gsub(":", "", key_part, fixed = TRUE)
            current_val <- substring(trim, attr(matches, "match.length") + 1)
        } else if (!is.null(current_key)) {
            current_val <- c(current_val, trim)
        }
    }

    if (!is.null(current_key)) {
        meta[[current_key]] <- trimws(paste(current_val, collapse = " "))
    }

    meta
}

empty_annotation <- function() {
    list(
        label = NULL,
        summary = NULL,
        details = character(),
        inputs = character(),
        outputs = character(),
        modifies = character(),
        tags = character(),
        section = NULL,
        publish = NULL
    )
}

trim_annotation_value <- function(value) {
    trimws(gsub("\\s+", " ", paste(value, collapse = " ")))
}

parse_annotation_vector <- function(value) {
    if (length(value) == 0) {
        return(character())
    }

    parts <- trimws(strsplit(paste(value, collapse = ","), ",", fixed = TRUE)[[1]])
    unique(parts[nzchar(parts)])
}

parse_annotation_flag <- function(value) {
    if (length(value) == 0) {
        return(NULL)
    }

    normalized <- tolower(trim_annotation_value(value))
    if (normalized %in% c("true", "yes", "y", "1")) {
        return(TRUE)
    }
    if (normalized %in% c("false", "no", "n", "0")) {
        return(FALSE)
    }
    NULL
}

parse_outliner_annotation <- function(comment_lines) {
    annotation <- empty_annotation()
    current_key <- NULL

    for (line in comment_lines) {
        text <- sub("^\\s*#\\s?", "", line)
        trimmed <- trimws(text)

        if (trimmed == "") {
            if (identical(current_key, "details")) {
                annotation$details <- c(annotation$details, "")
            }
            next
        }

        if (grepl("^@outliner_step\\b", trimmed)) {
            inline_label <- trimws(sub("^@outliner_step\\b", "", trimmed))
            if (nzchar(inline_label)) {
                annotation$label <- inline_label
            }
            current_key <- NULL
            next
        }

        if (grepl("^[A-Za-z][A-Za-z0-9_]*\\s*:", trimmed)) {
            key <- tolower(sub(":.*$", "", trimmed))
            value <- trimws(sub("^[A-Za-z][A-Za-z0-9_]*\\s*:\\s*", "", trimmed))
            current_key <- key

            if (key %in% c("label", "summary", "section")) {
                annotation[[key]] <- value
            } else if (key %in% c("inputs", "outputs", "modifies", "tags")) {
                annotation[[key]] <- parse_annotation_vector(value)
            } else if (key == "publish") {
                annotation$publish <- parse_annotation_flag(value)
            } else if (key == "details") {
                annotation$details <- if (nzchar(value)) value else character()
            }
            next
        }

        if (identical(current_key, "details")) {
            annotation$details <- c(annotation$details, sub("^[-*]\\s*", "", trimmed))
            next
        }

        if (identical(current_key, "label")) {
            annotation$label <- trim_annotation_value(c(annotation$label, trimmed))
        } else if (identical(current_key, "summary")) {
            annotation$summary <- trim_annotation_value(c(annotation$summary, trimmed))
        } else if (identical(current_key, "section")) {
            annotation$section <- trim_annotation_value(c(annotation$section, trimmed))
        } else if (identical(current_key, "publish")) {
            annotation$publish <- parse_annotation_flag(c(annotation$publish, trimmed))
        } else if (!is.null(current_key) && current_key %in% c("inputs", "outputs", "modifies", "tags")) {
            annotation[[current_key]] <- unique(c(annotation[[current_key]], parse_annotation_vector(trimmed)))
        }
    }

    annotation$details <- annotation$details[nzchar(annotation$details) | annotation$details == ""]
    annotation
}

collect_annotation_blocks <- function(code_lines, line_map, chunk_map = NULL) {
    blocks <- list()
    i <- 1
    total <- length(code_lines)

    while (i <= total) {
        if (!grepl("^\\s*#\\s*@outliner_step\\b", code_lines[i])) {
            i <- i + 1
            next
        }

        start <- i
        comment_lines <- character()

        while (i <= total) {
            line <- code_lines[i]
            if (grepl("^\\s*#", line)) {
                comment_lines <- c(comment_lines, line)
                i <- i + 1
                next
            }
            if (trimws(line) == "") {
                comment_lines <- c(comment_lines, "#")
                i <- i + 1
                next
            }
            break
        }

        end <- i - 1
        attached_line <- find_next_code_line(code_lines, end + 1)
        block <- parse_outliner_annotation(comment_lines)
        block$code_line_start <- start
        block$code_line_end <- end
        block$line_start <- line_map[start]
        block$line_end <- line_map[end]
        block$attached_line <- attached_line
        block$chunk <- if (!is.null(chunk_map) && length(chunk_map) >= start) chunk_map[start] else NA_character_
        blocks[[length(blocks) + 1]] <- block
    }

    blocks
}

find_next_code_line <- function(code_lines, start_index) {
    if (start_index > length(code_lines)) {
        return(NA_integer_)
    }

    for (i in seq.int(start_index, length(code_lines))) {
        line <- code_lines[i]
        if (trimws(line) == "") {
            next
        }
        if (grepl("^\\s*#", line)) {
            next
        }
        return(i)
    }

    NA_integer_
}
