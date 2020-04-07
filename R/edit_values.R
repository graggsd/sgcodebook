#' @export
edit_values <- function(data,
                        edit_cb,
                        id = "id",
                        variable = "variable",
                        from = "from",
                        to = "to") {

    args <- c(id, variable, from, to)
    # TODO add documentation for edit_values
    # TODO add error checking for arguments of edit_values
    # Check that arguments are colnames in edit_cb
    stopifnot(all(args %in% colnames(edit_cb)))
    stopifnot(id %in% colnames(data))

    # Filter out values that are not contained within data and present warnings
    # for these cases
    id_idx <- edit_cb[, id] %in% data[, id]
    if (any(!id_idx)) {
        ids <- edit_cb[!id_idx, id]
        w_1 <-
            paste0("The following identifiers specified in edit_cb are not ",
                   "contained within column ", id, " of data: ",
                   paste(ids, collapse = ", "))
        warning(w_1)
    }
    var_idx <- edit_cb[, variable] %in% colnames(data)
    if (any(!var_idx)) {
        vars <- edit_cb[!var_idx, var]
        w_2 <-
            paste0("The following column names specified in edit_cb column ",
                   variable, "are not contained as columns in data: ",
                   paste(vars, collapse = ", "))
        warning(w_2)
    }
    edit_cb <- edit_cb[(id_idx & var_idx), ]

    for (i in 1:nrow(edit_cb)) {
        dat_row_idx <- data[, id] == edit_cb[i, id]
        dat_col_idx <- colnames(data) == edit_cb[i, variable]
        # Check that data value is as expected
        if (data[dat_row_idx, dat_col_idx] != edit_cb[i, from]) {
            w_3 <- paste0("For observation ", edit_cb[i, id], " in data, ",
                          edit_cb[i, variable], " is equal to ",
                          data[dat_row_idx, dat_col_idx], ", but is listed ",
                          "as ", edit_cb[i, from], " in edit_cb.")
            warn(w_3)
        }

        data[dat_row_idx, dat_col_idx] <- edit_cb[i, to]
    }
    return(data)
}
