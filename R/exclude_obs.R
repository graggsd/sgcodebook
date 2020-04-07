#'@export
exlude_obs <- function(data, exclusion_cb, id = "id") {
    stopifnot(id %in% colnames(data))
    stopifnot(id %in% colnames(exclusion_cb))
    excl_idx <- data[, id] %in% exclusion_cb[, id]
    return(data[!excl_idx, ])
}
