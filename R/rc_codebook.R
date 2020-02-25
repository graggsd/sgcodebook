# Codebook certification -----------------------------------------------------

# Make sure that within each category, there are no duplicated "old" values
check_duplicated_mapping <- function(cb, cb_var_col, cb_val_old) {
    var_names <- unique(cb[, cb_var_col])
    for (var_name in var_names) {
        idx <- cb[, cb_var_col] == var_name
        if (anyDuplicated(cb[idx, cb_val_old])) {
            stop(paste0("There are duplicated 'old' values for the following",
                        " variable: ", var_name))
        }
    }
}

# Make sure there are no missing values within the codebook
check_missing_in_cb <-
    function(cb, cb_var_col, cb_val_old, cb_val_new) {
        for (cb_column in c(cb_var_col, cb_val_old, cb_val_new)) {
            if (anyNA(cb[, cb_column])) {
                stop(
                    paste0(
                        "The following column in the codebook contained NA ",
                        "values: ",
                        cb_column
                    )
                )
            }
        }
    }


# Check that input arguments are all contained as columns within the codebook
check_args_in_cb <-
    function(cb, cb_var_col, cb_val_old, cb_val_new) {
        args <- c(cb_var_col, cb_val_old, cb_val_new)
        missing_idx <- !(args %in% colnames(cb))
        if (any(missing_idx)) {
            stop(
                paste0(
                    "The following function arguments should be contained ",
                    "as column names in the codebook: ",
                    paste(args[missing_idx], collapse = ", ")
                )
            )
        }
    }

# Check codebook consistency
audit_codebook <- function(cb, cb_var_col, cb_val_old, cb_val_new) {

    check_duplicated_mapping(cb, cb_var_col, cb_val_old)
    check_missing_in_cb(cb, cb_var_col, cb_val_old, cb_val_new)
    check_args_in_cb(cb, cb_var_col, cb_val_old, cb_val_new)

}

# Helper functions to check cb and data expectations --------------------------

# Check that arguments meet expectations of function
check_args <- function(data, cb, cb_var_col, cb_val_old, cb_val_new) {
    stopifnot(is.data.frame(data))
    stopifnot(is.data.frame(cb))
    stopifnot(is.character(cb_var_col), length(cb_var_col) == 1)
    stopifnot(is.character(cb_val_old), length(cb_val_old) == 1)
    stopifnot(is.character(cb_val_new), length(cb_val_new) == 1)
}

# Checks that all variables listed in the codebook are
# contained as columns within the dataset
check_cbvars_in_data <- function(data, cb, cb_var_col) {
    vars_in_codebook <- unique(cb[, cb_var_col])
    idx <- vars_in_codebook %in% colnames(data)
    if (!all(idx)) {
        warning(paste0("The following variables from the codebook were not ",
                       "contained as columns in in the dataset: ",
                       paste(vars_in_codebook[!idx], collapse = ", ")))
    }
}

# Check that column classes are currently supported by the package


# Class handling -------------------------------------------------------------

# Function will initially only work with strings and otherwise throw and error
# Later, I will add functionality for other data types

# Get the class of all columns
get_col_classes <- function(data, cb, cb_var_col) {
    cols <- unique(cb[, cb_var_col])
    if (length(cols) > 1) {
        return(sapply(data[, cols], class))
    } else {
        return(class(data[, cols]))
    }
}

# Check if column classes are accepted
check_col_classes_supported <- function(data, cb, cb_var_col) {

    col_classes <- get_col_classes(data, cb, cb_var_col)

    # Character vector of data classes this package can currently handle
    # Within columns from data
    can_handle <- c("character")
    if (any(!(col_classes %in% can_handle))) {
        stop(
            paste0(
                "To use this function, columns in the 'data.frame'",
                " used in the 'data' argument that you wish to decode ",
                "must be of the the following classes: ",
                paste(can_handle, collapse = ", ")
            )
        )
    }
}

# Recode function --------------------------------------------------------

# Uses a codebook to rename values within the dataset

#' Recode values within a wide dataset
#'
#' Function to recode values within columns of a dataset using a custom
#' codebook
#'
#' The purpose of this function is to make recoding variables within a dataset,
#' easier through the use of a user-generated codebook. The intent is that
#' this codebook will be stored as a tabular data file, which may be more
#' easily edited, and subsequently imported during for analysis.
#' Storing the codebook in this way may also make reporting changes in
#' variable coding into others, who may not be familiar with R. Moreover,
#' delimited text files may be version controlled to document changes to
#' variable coding schemes throughout the analysis.
#'
#' Preliminary steps include importing the dataset as
#' a wide-format \code{data.frame}, which will
#' correspond to the \code{data} argument and generating a \code{data.frame}
#' that will serve as the codebook, \code{cb}.
#' \code{cb} must contain at least three
#' columns whose column names are specified in the \code{cb_var_col},
#' \code{cb_val_old}, and \code{cb_val_new} arguments of this function.
#' \code{cb_var_col} corresponds to a column within \code{cb}
#' that contains the names of variables to be edited. These variables will in
#' turn correspond to column names within \code{data}.
#' \code{cb_val_old} will specify
#' the name of a column in \code{cb} that contains old values within \code{data}
#' that may subsequently be recoded. \code{cb_val_old} corresponds to a column
#' in \code{cb} that contains new value for each old value.
#'
#'@param data a \code{data.frame} in wide-format such that each row
#'corresponds to a single participant and each column corresponds to a
#'single variable.
#'@param cb a \code{data.frame} containing at least one column
#',\code{cb_var_col}, corresponding to
#'variable names (column names) within \code{data},
#'one column, \code{cb_val_old}, corresponding to old values
#'within \code{data} that will be replaced,
#'and one column, \code{cb_val_new}, corresponding to new values that will be
#'used to replace the old.
#'@param cb_var_col a character vector of length 1 corresponding to a column
#'within \code{cb} containing variable names from \code{data}
#'@param cb_val_old a character vector of length 1 corresponding to a column
#'within \code{cb} containing old values within \code{data}
#'@param cb_val_new a character vector of length 1 corresponding to a column
#'within \code{cb} containing values that will be used to substitute
#'current values within \code{data}
#'#'@param cb_level_idx a character vector of length 1 corresponding to a column
#'within \code{cb} containing values that will be used to assign the order
#'of factor levels within recoded values of \code{data}
#'@return A \code{data.frame} similar to \code{data}, except that
#'values from some values will have been recoded based on \code{cb}
#'@export
#'@examples
#'cb <- data.frame(cb_var_col = c("x", "x", "y", "y"),
#'                 cb_val_old = c("0", "1", "0", "1"),
#'                 cb_val_new = c("no", "yes", "no", "yes"),
#'                 stringsAsFactors = FALSE)
#'data <- data.frame(PID = 1:4,
#'                   x = c("0", "1", "1", "0"),
#'                   y = c("1", "1", "0", "0"),
#'                   stringsAsFactors = FALSE)
#'out <- rc_codebook(data, cb, "cb_var_col", "cb_val_old", "cb_val_new")
#'out
rc_codebook <- function(data, cb, cb_var_col, cb_val_old, cb_val_new,
                        cb_level_idx = NULL) {

    # ----------------------------------------------------------------
    # Check for problems in function setup and display an appropriate
    # error message that will help the user fix the problem
    # ----------------------------------------------------------------

    # Checks that arguments are of appropriate class and length
    check_args(data, cb, cb_var_col, cb_val_old, cb_val_new)

    # Checks that 1) each new old value in the codebook is mapped to exactly
    # one new value, 2) there are no missing values in the codebook, 3)
    # makes sure that cb_* arguments are contained as
    # columns within the codebook
    audit_codebook(cb, cb_var_col, cb_val_old, cb_val_new)

    # Sends a warning if variables specified in the codebook are not
    # contained within the dataset
    check_cbvars_in_data(data, cb, cb_var_col)

    # Check if column classes are currently supported by the package
    check_col_classes_supported(data, cb, cb_var_col)

    # ===============================================================

    # Loop through unique variable names
    for (cb_var in unique(cb[, cb_var_col])) {

        # Create a subset of the codebook that pertains only to one variable
        cb_sub <- cb[cb[, cb_var_col] == cb_var, ]

        # Change class of column in dataset to character
        data[, cb_var] <- as.character(data[, cb_var])

        if (is.null(cb_level_idx)) {
            data[, cb_var] <- recode(x = data[, cb_var],
                                     from = cb_sub[, cb_val_old],
                                     to = cb_sub[, cb_val_new])
        } else if(all(is.na(cb_sub[, cb_level_idx]))) {
            data[, cb_var] <- recode(x = data[, cb_var],
                                     from = cb_sub[, cb_val_old],
                                     to = cb_sub[, cb_val_new])
        } else {
            data[, cb_var] <- rc_factor(x = data[, cb_var],
                                        from = cb_sub[, cb_val_old],
                                        to = cb_sub[, cb_val_new],
                                        level_idx = cb_sub[, cb_level_idx])
        }
    }
    return(data)
}

# Handle classes appropriately
# Deal with missingness
# Deal with factors appropriately
# Make appropriate and predictable conversions, while erroring out to prevent
# unexpected consequences
# Provide ways to specify the new classes for columns
# Generate warning messages when no adequate solution to convert a column class
# are available
# Create a new function to quickly go through two vectors and change values
# from one to the other

