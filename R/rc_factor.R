#' Recodes a character vector into a factor
#'
#' Recodes values of \code{x} based on pairs of values in \code{from} and
#' \code{to}, outputting a factor. Optionally allows the ordering of factor
#' levels based on \code{level_idx}.
#'
#' @param x vector that will be recoded and turned into a factor
#' @param from vector containing all unique values in \code{x}
#' @param to vector containing values that will replace those in \code{from} -
#' should be ordered in a manner so that each value in \code{to} occurs in this
#' vector as the corresponding value in \code{from}
#' @param level_idx vector specifying the sequence of levels in
#' \code{rc_factor}'s output - position of each value should be indexed to
#' corresponding value in \code{to} and \code{from}.
#' @return A vector or class \code{factor}
#' @examples
#' x <- as.character(1:10)
#' from <- as.character(1:10)
#' to <- letters[1:10]
#' level_idx <- rev(1:10)
#' rc_char(x, from, to, level_idx)
rc_factor <- function(x, from, to, level_idx = NULL) {

    # Convert some input arguments to character
    x <- as.character(x)
    from <- as.character(from)
    to <- as.character(to)

    lvl_idx_req <- "level_idx must be numeric or fully coercible character vector"

    # Check for NA values in from -------------------------------------
    if(any(is.na(from))) {
        stop("Function currently does not support NA values in argument 'from'")
    }
    # Check non-unique mapping ----------------------------------------
    if(any(duplicated(from))) {
        stop("Function does not accept duplicated values in 'from'")
    }

    if (any(!(x %in% from) & !is.na(x))) {
        stop("All non-missing values of 'x' must be contained in 'from'")
    }

    if (length(from) != length(to)) {
        stop("The length of 'from' must equal the length of 'to'")
    }

    if (!is.null(level_idx)) {

        if (length(unique(c(length(from), length(to), length(level_idx)))) > 1) {
            stop("The lengths of 'from', 'to', and 'level_idx must be equal")
        }

        if(anyDuplicated(level_idx[!duplicated(to)])) {
            stop("'level_idx' must be unique for each unique value of 'to'")
        }

        if(!is.numeric(level_idx) & !is.character(level_idx)) {
            stop()
        }

        if(is.character(level_idx)) {
            tryCatch(
                level_idx <- as.numeric(level_idx),
                warning = function(warn) stop(lvl_idx_req, .call = FALSE),
                error = function(err) stop(lvl_idx_req, .call = FALSE)
            )
        }
    }

    # Check that all NA values are appropriate
    if (!is.null(level_idx)) {
        if (!(all(is.na(to) == is.na(level_idx)))) {
            er <-
                paste0("Any 'NA' values in 'to' must have paired 'NA' values ",
                       " vectors specifying factor levels, and vice versa.")
        }
        # Any NA values in to must match the position of those in level_idx
        stopifnot()

    }

    # To avoid self-refrencing and creating unexpected behavior
    new_x <- x

    for (i in 1:length(from)) {
        new_x[x == from[i]] <- to[i]
    }
    if (is.null(level_idx)) {
        new_x <- factor(new_x, levels = unique(to))
    } else {

        # Any NA values in to must match the position of those in level_idx
        stopifnot(all(is.na(to) == is.na(level_idx)))

        # Remove any NA values before determining level order
        level_idx <- level_idx[!is.na(level_idx)]
        to <- to[!is.na(to)]

        idx <- order(level_idx)
        new_x <- factor(new_x, levels = unique(to[idx]))
    }

    return(new_x)
}
