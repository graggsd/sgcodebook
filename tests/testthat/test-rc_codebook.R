context("rc_codebook")

# ----------------------------------------------------------
# Some templates to work with
# ----------------------------------------------------------
cb <- data.frame(cb_var_col = c("x", "x", "y", "y"),
                 cb_val_old = c("0", "1", "0", "1"),
                 cb_val_new = c("no", "yes", "no", "yes"),
                 stringsAsFactors = FALSE)

data <- data.frame(PID = 1:4,
                   x = c("0", "1", "1", "0"),
                   y = c("1", "1", "0", "0"),
                   stringsAsFactors = FALSE)

expected_out <- data.frame(
    PID = 1:4,
    x = c("no", "yes", "yes", "no"),
    y = c("yes", "yes", "no", "no"),
    stringsAsFactors = FALSE
)
out <- rc_codebook(data, cb, "cb_var_col", "cb_val_old", "cb_val_new")

# ============================================================

test_that("basic functionality intact", {
    expect_equal(out, expected_out)
})

test_that("works with NA values embedded in 'data'", {

    data <- data.frame(PID = 1:4,
                       x = c("0", "1", NA, "0"),
                       y = c(NA, "1", "0", "0"),
                       stringsAsFactors = FALSE)
    expected_out <- data.frame(PID = 1:4,
                               x = c("no", "yes", NA, "no"),
                               y = c(NA, "yes", "no", "no"),
                               stringsAsFactors = FALSE)
    out <- rc_codebook(data, cb, "cb_var_col", "cb_val_old", "cb_val_new")
    expect_equal(out, expected_out)

})

test_that("works as expected with factors", {
    cb <- data.frame(cb_var_col = c("x", "x", "y", "y"),
                     cb_val_old = c("0", "1", "0", "1"),
                     cb_val_new = c("no", "yes", "no", "yes"),
                     cb_level_idx = c(1, 0, 1, 0),
                     stringsAsFactors = FALSE)
    expected_out <- data.frame(PID = 1:4,
                               x = factor(c("no", "yes", "yes", "no"),
                                          levels = c("yes", "no")),
                               y = factor(c("yes", "yes", "no", "no"),
                                          levels = c("yes", "no")),
                               stringsAsFactors = FALSE)
    out <- rc_codebook(data, cb, "cb_var_col", "cb_val_old", "cb_val_new",
                       cb_level_idx = "cb_level_idx")
    expect_equal(out, expected_out)
})

test_that("assigns NA values when cb_val_new is missing", {
    cb <- data.frame(cb_var_col = c("x", "x", "y", "y"),
                     cb_val_old = c("0", "1", "0", "1"),
                     cb_val_new = c(NA, "yes", "no", "yes"),
                     stringsAsFactors = FALSE)
    expected_out <- data.frame(
        PID = 1:4,
        x = c(NA, "yes", "yes", NA),
        y = c("yes", "yes", "no", "no"),
        stringsAsFactors = FALSE
    )
    out <- rc_codebook(data, cb, "cb_var_col", "cb_val_old", "cb_val_new")
    expect_equal(out, expected_out)
})
