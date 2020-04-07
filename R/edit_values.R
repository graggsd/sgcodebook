#' @export
edit_values <- function(data,
                        codebook,
                        id = "id",
                        variable = "variable",
                        from = "from",
                        to = "to") {

    args <- c(id, variable, from, to)
    # TODO add documentation for edit_values
    # TODO add error checking for arguments of edit_values
    # Check that arguments are colnames in codebook
    stopifnot(all(args %in% colnames(codebook)))
    stopifnot(id %in% colnames(data))

    # Filter out values that are not contained within data and present warnings
    # for these cases
    id_idx <- codebook[, id] %in% data[, id]
    if (any(!id_idx)) {
        ids <- codebook[!id_idx, id]
        w_1 <-
            paste0("The following identifiers specified in codebook are not ",
                   "contained within column ", id, " of data: ",
                   paste(ids, collapse = ", "))
        warning(w_1)
    }
    var_idx <- codebook[, variable] %in% colnames(data)
    if (any(!var_idx)) {
        vars <- codebook[!var_idx, var]
        w_2 <-
            paste0("The following column names specified in codebook column ",
                   variable, "are not contained as columns in data: ",
                   paste(vars, collapse = ", "))
        warning(w_2)
    }
    codebook <- codebook[(id_idx & var_idx), ]

    for (i in 1:nrow(codebook)) {
        dat_row_idx <- data[, id] == codebook[i, id]
        dat_col_idx <- colnames(data) == codebook[i, variable]
        # Check that data value is as expected
        if (data[dat_row_idx, dat_col_idx] != codebook[i, from]) {
            w_3 <- paste0("For observation ", codebook[i, id], " in data, ",
                          codebook[i, variable], " is equal to ",
                          data[dat_row_idx, dat_col_idx], ", but is listed ",
                          "as ", codebook[i, from], " in codebook.")
            warn(w_3)
        }

        data[dat_row_idx, dat_col_idx] <- codebook[i, to]
    }
    return(data)
}
