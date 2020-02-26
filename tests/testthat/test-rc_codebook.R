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


# ----------------------------------------------------------
# Character values
# ----------------------------------------------------------

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

test_that("works with character values when combining variables", {
    cb <- data.frame(cb_var_col = c("x", "x", "y", "y"),
                     cb_val_old = c("0", "1", "0", "1"),
                     cb_val_new = c("no", "no", "no", "yes"),
                     stringsAsFactors = FALSE)
    expected_out <- data.frame(
        PID = 1:4,
        x = c("no", "no", "no", "no"),
        y = c("yes", "yes", "no", "no"),
        stringsAsFactors = FALSE
    )
    out <- rc_codebook(data, cb, "cb_var_col", "cb_val_old", "cb_val_new")
    expect_equal(out, expected_out)
})

# ----------------------------------------------------------
# Factors
# ----------------------------------------------------------

data <- data.frame(PID = 1:6,
                   x = as.character(rep(1:3, 2)),
                   y = as.character(rep(1:3, 2)),
                   stringsAsFactors = FALSE)
cb <- data.frame(cb_var_col = c("x", "x", "x", "y", "y", "y"),
                 cb_val_old = c("1", "2", "3", "1", "2", "3"),
                 cb_val_new = c("A", "B", "C", "X", "Y", "Z"),
                 cb_level_idx = c(1, 2, 3, 1, 2, 3),
                 stringsAsFactors = FALSE)
expected_out <- data.frame(
    PID = 1:6,
    x = factor(c("A", "B", "C", "A", "B", "C"),
               levels = c("A", "B", "C")),
    y = factor(c("X", "Y", "Z", "X", "Y", "Z"),
               levels = c("X", "Y", "Z")),
    stringsAsFactors = FALSE
)
out <- rc_codebook(data, cb, "cb_var_col", "cb_val_old", "cb_val_new",
                   cb_level_idx = "cb_level_idx")

test_that("works as expected with factors", {
    # Test the baseline above
    expect_equal(out, expected_out)

    # reverse order of factors
    cb <- data.frame(cb_var_col = c("x", "x", "x", "y", "y", "y"),
                     cb_val_old = c("1", "2", "3", "1", "2", "3"),
                     cb_val_new = c("A", "B", "C", "X", "Y", "Z"),
                     cb_level_idx = c(1, 2, 3, 3, 2, 1),
                     stringsAsFactors = FALSE)
    expected_out <- data.frame(
        PID = 1:6,
        x = factor(c("A", "B", "C", "A", "B", "C"),
                   levels = c("A", "B", "C")),
        y = factor(c("X", "Y", "Z", "X", "Y", "Z"),
                   levels = c("Z", "Y", "X")),
        stringsAsFactors = FALSE
    )
    out <- rc_codebook(data, cb, "cb_var_col", "cb_val_old", "cb_val_new",
                       cb_level_idx = "cb_level_idx")
    expect_equal(out, expected_out)

})

test_that("can combine values when generating factors", {
    # reverse order of factors
    cb <- data.frame(cb_var_col = c("x", "x", "x", "y", "y", "y"),
                     cb_val_old = c("1", "2", "3", "1", "2", "3"),
                     cb_val_new = c("A", "B", "B", "X", "Y", "Z"),
                     cb_level_idx = c(1, 2, 2, 3, 2, 1),
                     stringsAsFactors = FALSE)
    expected_out <- data.frame(
        PID = 1:6,
        x = factor(c("A", "B", "B", "A", "B", "B"),
                   levels = c("A", "B")),
        y = factor(c("X", "Y", "Z", "X", "Y", "Z"),
                   levels = c("Z", "Y", "X")),
        stringsAsFactors = FALSE
    )
    out <- rc_codebook(data, cb, "cb_var_col", "cb_val_old", "cb_val_new",
                       cb_level_idx = "cb_level_idx")
    expect_equal(out, expected_out)
})
