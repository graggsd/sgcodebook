# Codebook certification -----------------------------------------------------

# Make sure that within each category, there are no duplicated "old" values
check_duplicated_mapping <- function(codebook, variable, from) {
    var_names <- unique(codebook[, variable])
    for (var_name in var_names) {
        idx <- codebook[, variable] == var_name
        if (anyDuplicated(codebook[idx, from])) {
            stop(paste0("There are duplicated 'old' values for the following",
                        " variable: ", var_name))
        }
    }
}

# Make sure there are no missing values within the codebook
check_missing_in_codebook <-
    function(codebook, variable, from) {
        for (codebook_column in c(variable, from)) {
            if (anyNA(codebook[, codebook_column])) {
                stop(
                    paste0(
                        "The following column in the codebook contained NA ",
                        "values: ",
                        codebook_column
                    )
                )
            }
        }
    }


# Check that input arguments are all contained as columns within the codebook
check_args_in_codebook <-
    function(codebook, variable, from, to) {
        args <- c(variable, from, to)
        missing_idx <- !(args %in% colnames(codebook))
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
audit_codebook <- function(codebook, variable, from, to) {
    check_duplicated_mapping(codebook, variable, from)
    check_missing_in_codebook(codebook, variable, from)
    check_args_in_codebook(codebook, variable, from, to)
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
#' that will serve as the codebook, \code{codebook}.
#' \code{codebook} must contain at least three
#' columns whose column names are specified in the \code{variable},
#' \code{from}, and \code{to} arguments of this function.
#' \code{variable} corresponds to a column within \code{codebook}
#' that contains the names of variables to be edited. These variables will in
#' turn correspond to column names within \code{data}.
#' \code{from} will specify
#' the name of a column in \code{codebook} that contains old values within \code{data}
#' that may subsequently be recoded. \code{from} corresponds to a column
#' in \code{codebook} that contains new value for each old value.
#'
#'@param data a \code{data.frame} with columns containing unique variables
#'@param codebook a \code{data.frame} containing at least three columns
#'corresponding to: 1) variable (column) names within \code{data}, 2) old
#'values within \code{data} to be replaced, and 3) new values that will be
#'used to replace the old. Optionally contains a third column specifiying
#'factor levels. Values within columns of \code{codebook} are linked by row.
#'@param variable a character string containing the column name
#'within \code{codebook} housing variable names (columns) within \code{data}
#'to be recoded
#'@param from a character string containing the column name
#'within \code{codebook} housing old values from \code{data}
#'@param to a character string containing the column name
#'within \code{codebook} housing new values that will be used
#'as substitutes for current values within \code{data}
#'@param factor_levels a character string containing the column name
#'within \code{codebook} housing values that will be used to assign the order
#'of factor levels within recoded values of \code{data}
#'@return A \code{data.frame} similar to \code{data}, except that
#'specified values will have been recoded based on \code{codebook}
#'@export
#'@examples
#'codebook <- data.frame(variable = c("x", "x", "y", "y"),
#'                 from = c("0", "1", "0", "1"),
#'                 to = c("no", "yes", "no", "yes"),
#'                 stringsAsFactors = FALSE)
#'data <- data.frame(PID = 1:4,
#'                   x = c("0", "1", "1", "0"),
#'                   y = c("1", "1", "0", "0"),
#'                   stringsAsFactors = FALSE)
#'out <- rc_codebook(data, codebook, "variable", "from", "to")
#'out
rc_codebook <- function(data, codebook,
                        variable = NULL, from = NULL, to = NULL,
                        factor_levels = NULL) {

    # QUESTION could this be simplified by moving some of this boolean argument stuff into a helper function
    # TODO consider the above question and implement with other functions to make this package easier to use

    # ==========================================================
    # Find default columns in codebook
    # ==========================================================
    argument_names <- c("variable", "from", "to")
    arg_list <- list(variable, from, to)
    names(arg_list) <- argument_names

    # Throw error if not NULL or length 1
    if (any(lapply(arg_list, length) > 1)) {
        er <-
            paste0(
                "The following arguments must be of length 1 or NULL:\n",
                "'variable', 'from', and 'to'."
            )
        stop(er)
    }

    # Throw error if not all of class character, numeric, or NULL
    arg_classes <- lapply(arg_list, class)
    correct_class_idx <-
        arg_classes %in% c("character", "numeric", "NULL")

    if (!all(correct_class_idx)) {
        b_args <- arg_classes[!correct_class_idx]
        report <-
            paste(paste0("'", names(b_args), "' was class '", b_args, "'"),
                  collapse = "\n")
        er <-
            paste0(
                "The arguments 'variable', 'from', and 'to' should be of\n",
                "class: numeric, character, or NULL.\n",
                report
            )
        stop(er)
    }

    # Handle null arguments
    null_arg_idx <- unlist(lapply(arg_list, is.null))
    null_arg_names <- names(null_arg_idx)[null_arg_idx]

    # Throw error if assigned argument does not exist as a column in codebook
    not_null_arg_names <- unlist(arg_list)

    if (!(all(not_null_arg_names %in% colnames(codebook)))) {
        er <-
            paste0(
                "If the arguments 'variable', 'from', or 'to' are assigned\n",
                "a value, that value must be contained as a column name in\n",
                "codebook."
            )
        stop(er)
    }

    if (any(null_arg_idx)) {
        m1 <- paste0("The following arguments were left NULL: ",
                     paste(paste0("'", null_arg_names, "'"), collapse = "; "),
                     "\n")
        message(m1)

        # If all of the default codebook column names ('variable', 'from', and 'to')
        # exist as column names in codebook, then default to these names
        # Else, the argument 'variable' will assume the value of the
        # first column in codebook, 'from' will take on the second column name
        # and 'to' will take on the third
        if (all(null_arg_names %in% colnames(codebook))) {
            m2 <- paste0(
                "Because each of the aforementioned arguments ",
                "had a matching column name in 'codebook', \nthe value for ",
                "each argument will be assigned as follows:\n"
            )
            m3 <-
                paste(paste0("'", null_arg_names, "' = '", null_arg_names, "'"),
                      collapse = "; ")
            message(m2, m3)
            for (null_arg in null_arg_names) {
                arg_list[null_arg] <- null_arg
            }
        } else {
            m2 <- paste0(
                "Because one or more of the aforementioned arguments ",
                "had no matching column name\nin 'codebook', the value for ",
                "each argument will be assigned according to the first\n",
                "three column names in 'codebook'\n"
            )
            m3 <-
                paste(paste0("'", null_arg_names, "' = '",
                             colnames(codebook)[which(null_arg_idx)],
                             "'"),
                      collapse = "; ")
            message(m2, m3)
            for (i in which(null_arg_idx)) {
                arg_list[i] <- colnames(codebook)[i]
            }
        }
    }

    # Throw error if there are duplicated values
    if (any(duplicated(unlist(arg_list)))) {
        m1 <-
            paste0(
                "There were duplicated values found among the arguments ",
                "'variable', 'from',\n and 'to'. Each of these arguments ",
                "must be assigned a unique value and represent a\n column",
                " name in 'codebook'. \n\nThe arguments were assigned as follows",
                ":\n"
            )
        m2 <-
            paste(paste0("'", names(arg_list), "' = '", unlist(arg_list), "'"),
                  collapse = "; ")
        m3 <-
            paste0(
                "Please change the values assigned to 'variable', 'from',",
                " and 'to' so that each\n specifies a unique column in ",
                "'codebook'."
            )
        stop(m1, m2, "\n\n", m3)
    }

    variable <- arg_list["variable"]
    from <- arg_list["from"]
    to  <- arg_list["to"]

    # ==========================================================
    # Convert arguments
    # ==========================================================
    variable <- as.character(variable)
    from <- as.character(from)
    to <- as.character(to)
    if (!is.null(factor_levels)) factor_levels <- as.character(factor_levels)

    # ----------------------------------------------------------------
    # Check for problems in function setup and display an appropriate
    # error message that will help the user fix the problem
    # ----------------------------------------------------------------

    # Check that arguments are of appropriate class and length
    stopifnot(is.data.frame(data))
    stopifnot(is.data.frame(codebook))
    stopifnot(length(variable) == 1)
    stopifnot(length(from) == 1)
    stopifnot(length(to) == 1)

    # Checks that 1) each new old value in the codebook is mapped to exactly
    # one new value, 2) there are no missing values in the codebook, 3)
    # makes sure that codebook_* arguments are contained as
    # columns within the codebook
    audit_codebook(codebook, variable, from, to)

    # Check that vars in codebooks are contained within the dataset
    # and remove those that aren't
    vars_in_codebook <- codebook[, variable]
    idx <- vars_in_codebook %in% colnames(data)
    if (!all(idx)) {
        warning(paste0("The following values from the column 'variable' in ",
                       "'codebook' were not contained as columns in 'data': ",
                       paste(unique(vars_in_codebook[!idx]), collapse = ", ")))

        codebook <- codebook[idx, ]
    }

    # ===============================================================

    # Loop through unique variable names
    for (codebook_var in unique(codebook[, variable])) {

        # Create a subset of the codebook that pertains only to one variable
        codebook_sub <- codebook[codebook[, variable] == codebook_var, ]

        # Change class of column in dataset to character
        data[, codebook_var] <- as.character(data[, codebook_var])

        # Give warning if any values in data are not contained as values in
        # the 'from' column of codebook_sub
        idx <- (data[, codebook_var] %in% codebook_sub[, from]) |
            is.na(data[, codebook_var])
        if (!all(idx)) {
            warning(paste0("The following values within column '", codebook_var,
                           "' of data were not contained in the 'from' ",
                           "column of 'codebook': ",
                           paste(unique(data[!idx, codebook_var]), collapse = "; ")))
        }

        if (is.null(factor_levels)) {
            data[, codebook_var] <- rc_char(x = data[, codebook_var],
                                     from = codebook_sub[, from],
                                     to = codebook_sub[, to],
                                     warn = FALSE)
        } else if(all(is.na(codebook_sub[, factor_levels]))) {
            data[, codebook_var] <- rc_char(x = data[, codebook_var],
                                     from = codebook_sub[, from],
                                     to = codebook_sub[, to],
                                     warn = FALSE)
        } else {
            data[, codebook_var] <- rc_factor(x = data[, codebook_var],
                                        from = codebook_sub[, from],
                                        to = codebook_sub[, to],
                                        level_idx = codebook_sub[, factor_levels])
        }
    }
    return(data)
}

