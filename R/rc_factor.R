#' Recodes a character vector into a factor
#'
#' Recodes values of \code{x} based on pairs of values in \code{from} and
#' \code{to}, outputting a factor. Optionally allows the ordering of factor
#' levels based on \code{level_idx}.
#'
#' @param x character vector that will be recoded and turned into a factor
#' @param from character vector containing each unique value in \code{x}
#' @param to character vector containing new values that will be used
#' as a substitute for values in \code{from}
#' @param level_idx The order in which \code{to} will be incorporated into the
#' new factor
#' @return A factor
#' @examples
#' x <- as.character(1:10)
#' from <- as.character(1:10)
#' to <- letters[1:10]
#' level_idx <- rev(1:10)
#' recode(x, from, to, level_idx)
rc_factor <- function(x, from, to, level_idx = NULL) {

    # Check for NA values in from -------------------------------------
    # may change in future
    if(any(is.na(from))) {
        stop("Function currently does not support NA values in argument 'from'")
    }

    # Check non-unique mapping ----------------------------------------
    if(any(duplicated(from))) {
        stop("Function does not accept duplicated values in 'from'")
    }
    if(anyDuplicated(level_idx[!duplicated(to)])) {
        stop("'level_idx' must be unique for each unique value of 'to'")
    }

    if (is.null(level_idx) & length(from) != length(to)) {
        stop("The length of 'from' must equal the length of 'to'")
    } else if(!is.null(level_idx) &
              !all.equal(length(from), length(to), length(level_idx))) {
        stop("The lengths of 'from', 'to', and 'level_idx must be equal")
    }

    if (!is.null(level_idx) & !is.numeric(level_idx)) {
        stop("level_idx must be of class numeric")
    }

    if (any(!(x %in% from))) {
        stop("All values of 'x' must be contained in 'from'")
    }

    new_x <- x

    for (i in 1:length(from)) {
        new_x[x == from[i]] <- to[i]
    }
    if (is.null(level_idx)) {
        new_x <- factor(new_x, levels = unique(to))
    } else {
        idx <- order(level_idx)
        new_x <- factor(new_x, levels = unique(to[idx]))
    }

    return(new_x)
}
