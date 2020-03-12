#' Recodes exact matches in a character string
#'
#' Finds values in \code{x} that are equivalent to values in
#' \code{from} and substitutes them with corresponding values in \code{to}.
#'
#' If only a single value is included in \code{to}, then all values in
#' \code{x} matching
#' values in \code{from} will be changed to the single value in \code{to}.
#' Otherwise, the lengths of \code{from} and \code{to} must be equal, and there
#' can be no duplicated values in \code{from}.
#'
#' Note that when anticipating output of \code{recode}, this function
#' automatically converts all input arguments to character vectors in the
#' first step.
#'
#' @param x A vector containing values that will be recoded based on
#' \code{from} and \code{to}
#' @param from A vector containing the values within \code{x} that will
#' be recoded
#' @param to A vector containing the corresponding value (or values) that
#' \code{from} will be converted to
#' @param warn If 'TRUE', a warning will be generated when values in 'x'
#' are not contained in 'from
#' @param default_NA If 'TRUE', \code{recode} will convert values from \code{x}
#' not in \code{from} to \code{NA}
#' @return A character vector, derived from \code{x}, with 0 or more values
#' substituted for new ones
#' @export
#' @examples
#' x <- as.character(1:10)
#' from <- as.character(1:10)
#' to <- letters[1:10]
#' recode(x, from, to)
recode <- function(x, from, to, warn = TRUE, default_NA = FALSE) {

    # Input coercion ----------------------------------------
    x <- as.character(x)
    from <- as.character(from)
    to <- as.character(to)

    # ==========================================================
    # Argument checking
    # ==========================================================

    # Make sure from and to lengths indexable ----------------------
    if(!(length(from) == length(to) | length(to) == 1)) {
        stop(paste0("The length of 'from' must be equal to the length of 'to' ",
                    "OR 'to' must be of length 1"),
             .Call = FALSE)
    }

    # Check for NA values in from -------------------------------------
    # may change in future
    if(any(is.na(from))) {
        stop("Function currently does not support NA values in argument 'from'",
             .Call = FALSE)
    }

    # Check non-unique mapping ----------------------------------------
    if(any(duplicated(from))) {
        stop("Function does not accept duplicated values in 'from'",
             .Call = FALSE)
    }

    # Set index for values in x, not contained in from, to NA
    excl_idx <- !(x %in% from | is.na(x))

    if (sum(excl_idx) > 0) {
        # Identify values in x, not contained in from
        if (warn) {
            warning(
                paste0("The following values in 'x' are not contained",
                       " in 'from':",
                       paste(unique(x[excl_idx]),
                             collapse = "; ")
                )
            )
        }
        # Set values in x, not contained in from, to NA
        if (default_NA) {
            x[excl_idx] <- NA
        }
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

    return(new_x)
}


