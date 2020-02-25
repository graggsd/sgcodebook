#' Recodes exact matches in a character string
#'
#' Finds values in \code{x} that are equivalent to values in
#' \code{from} and substitutes them with corresponding values in \code{to}. If only
#' a single value is included in \code{to}, then all values in \code{x} matching
#' values in \code{from} will be changed to the single value in \code{to}.
#' Otherwise, the lengths of \code{from} and \code{to} must be equal, and there
#' can be no duplicated values in \code{from}.
#'
#' @param x A character vector containing values that will be recoded based on
#' \code{from} and \code{to}
#' @param from A character vector containing the values within \code{x} that will
#' be recoded
#' @param to The corresponding value (or values) that \code{from} will be
#' converted to
#' @return A character vector, derived from \code{x}, with 0 or more values
#' substituted for new ones
#' @export
#' @examples
#' x <- as.character(1:10)
#' from <- as.character(1:10)
#' to <- letters[1:10]
#' recode(x, from, to)
recode <- function(x, from, to) {

    # ==========================================================
    # Argument checking
    # ==========================================================

    # Arguments must be of the same class ----------------------
    if (length(unique(c(class(x), class(from), class(to)))) > 1) {
        stop(paste0("Arguments must all be of the same class"))
    }

    # Restrict argument class ----------------------
    allowed_classes <- c("character")
    if (!all(c(class(x), class(from), class(to)) %in% allowed_classes)) {
        stop(paste0("Arguments must be one of the following classes: ",
                    paste(allowed_classes, collapse = ", ")))
    }

    # Make sure from and to lengths indexable ----------------------
    if(!(length(from) == length(to) | length(to) == 1)) {
        stop(paste0("The length of 'from' must be equal to the length of 'to' ",
                    "OR 'to' must be of length 1"))
    }

    # Check for NA values in from -------------------------------------
    # may change in future
    if(any(is.na(from))) {
        stop("Function currently does not support NA values in argument 'from'")
    }

    # Check non-unique mapping ----------------------------------------
    if(any(duplicated(from))) {
        stop("Function does not accept duplicated values in 'from'")
    }

    # ==========================================================
    # Function components
    # ==========================================================

    idx <- which(from %in% x)
    from <- from[idx]
    new_x <- x

    if (length(to) > 1) {
        # subset based on values of from contained in x
        to <- to[idx]
        for (i in 1:length(from)) {
            new_x[x == from[i]] <- to[i]
        }
    } else {
        for (i in 1:length(from)) {
            new_x[x == from[i]] <- to
        }
    }

    # Functonality may be implemented later
    # if (class(new_x) != class(x)) {
    #     warning(paste0("Function 'recode' coherced class change from ",
    #                    class(x), " to ", class(new_x)))
    # }

    return(new_x)
}


