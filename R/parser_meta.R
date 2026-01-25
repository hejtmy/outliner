#' Extract Metadata from Function
#'
#' Parses the first expression of a function body to check for
#' a string literal containing colon-prefixed metadata.
#'
#' @param fun A function object.
#' @return A named list with metadata keys (e.g., name, short, long), or NULL if not found.
#' @export
extract_meta <- function(fun) {
    if (!is.function(fun)) {
        return(NULL)
    }

    b <- body(fun)
    # Body might be a straight call or a braced list of calls.
    # If it's a `{`, the content is in subsequent elements.
    # usually body(fun)[[1]] is `{`, [[2]] is the first line.

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

    return(NULL)
}

#' Parse Metadata String
#'
#' @param text The string containing YAML-like metadata.
#' @return A named list.
#' @keywords internal
parse_meta_string <- function(text) {
    lines <- strsplit(text, "\n")[[1]]
    meta <- list()
    current_key <- NULL
    current_val <- ""

    for (line in lines) {
        trim <- trimws(line)
        if (trim == "") next

        # Check for key starting with :
        if (grepl("^:[a-zA-Z]+:", trim)) {
            # Save previous
            if (!is.null(current_key)) {
                meta[[current_key]] <- trimws(current_val)
            }

            # Start new
            matches <- regexpr("^:[a-zA-Z]+:", trim)
            key_part <- regmatches(trim, matches)
            current_key <- gsub(":", "", key_part)
            current_val <- substring(trim, attr(matches, "match.length") + 1)
        } else {
            # Append to current
            if (!is.null(current_key)) {
                current_val <- paste(current_val, trim, sep = " ")
            }
        }
    }

    # Last one
    if (!is.null(current_key)) {
        meta[[current_key]] <- trimws(current_val)
    }

    return(meta)
}
