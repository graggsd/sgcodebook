context("rc_codebook")

# ----------------------------------------------------------
# Some templates to work with
# ----------------------------------------------------------
codebook <- data.frame(variable = c("x", "x", "y", "y"),
                 from = c("0", "1", "0", "1"),
                 to = c("no", "yes", "no", "yes"),
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
out <- rc_codebook(data, codebook, "variable", "from", "to")

# ============================================================

# ----------------------------------------------------------
# Output character
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
    out <- rc_codebook(data, codebook, "variable", "from", "to")
    expect_equal(out, expected_out)

})

test_that("assigns NA values when to is missing", {
    codebook <- data.frame(variable = c("x", "x", "y", "y"),
                     from = c("0", "1", "0", "1"),
                     to = c(NA, "yes", "no", "yes"),
                     stringsAsFactors = FALSE)
    expected_out <- data.frame(
        PID = 1:4,
        x = c(NA, "yes", "yes", NA),
        y = c("yes", "yes", "no", "no"),
        stringsAsFactors = FALSE
    )
    out <- rc_codebook(data, codebook, "variable", "from", "to")
    expect_equal(out, expected_out)
})

test_that("works when combining variables", {
    codebook <- data.frame(variable = c("x", "x", "y", "y"),
                     from = c("0", "1", "0", "1"),
                     to = c("no", "no", "no", "yes"),
                     stringsAsFactors = FALSE)
    expected_out <- data.frame(
        PID = 1:4,
        x = c("no", "no", "no", "no"),
        y = c("yes", "yes", "no", "no"),
        stringsAsFactors = FALSE
    )
    out <- rc_codebook(data, codebook, "variable", "from", "to")
    expect_equal(out, expected_out)
})

# ----------------------------------------------------------
# Output Factor
# ----------------------------------------------------------

data <- data.frame(PID = 1:6,
                   x = as.character(rep(1:3, 2)),
                   y = as.character(rep(1:3, 2)),
                   stringsAsFactors = FALSE)
codebook <- data.frame(variable = c("x", "x", "x", "y", "y", "y"),
                 from = c("1", "2", "3", "1", "2", "3"),
                 to = c("A", "B", "C", "X", "Y", "Z"),
                 factor_levels = c(1, 2, 3, 1, 2, 3),
                 stringsAsFactors = FALSE)
expected_out <- data.frame(
    PID = 1:6,
    x = factor(c("A", "B", "C", "A", "B", "C"),
               levels = c("A", "B", "C")),
    y = factor(c("X", "Y", "Z", "X", "Y", "Z"),
               levels = c("X", "Y", "Z")),
    stringsAsFactors = FALSE
)
out <- rc_codebook(data, codebook, "variable", "from", "to",
                   factor_levels = "factor_levels")

test_that("works as expected with factors", {
    # Test the baseline above
    expect_equal(out, expected_out)

    # reverse order of factors
    codebook <- data.frame(variable = c("x", "x", "x", "y", "y", "y"),
                     from = c("1", "2", "3", "1", "2", "3"),
                     to = c("A", "B", "C", "X", "Y", "Z"),
                     factor_levels = c(1, 2, 3, 3, 2, 1),
                     stringsAsFactors = FALSE)
    expected_out <- data.frame(
        PID = 1:6,
        x = factor(c("A", "B", "C", "A", "B", "C"),
                   levels = c("A", "B", "C")),
        y = factor(c("X", "Y", "Z", "X", "Y", "Z"),
                   levels = c("Z", "Y", "X")),
        stringsAsFactors = FALSE
    )
    out <- rc_codebook(data, codebook, "variable", "from", "to",
                       factor_levels = "factor_levels")
    expect_equal(out, expected_out)

})

test_that("can combine values when generating factors", {
    # reverse order of factors
    codebook <- data.frame(variable = c("x", "x", "x", "y", "y", "y"),
                     from = c("1", "2", "3", "1", "2", "3"),
                     to = c("A", "B", "B", "X", "Y", "Z"),
                     factor_levels = c(1, 2, 2, 3, 2, 1),
                     stringsAsFactors = FALSE)
    expected_out <- data.frame(
        PID = 1:6,
        x = factor(c("A", "B", "B", "A", "B", "B"),
                   levels = c("A", "B")),
        y = factor(c("X", "Y", "Z", "X", "Y", "Z"),
                   levels = c("Z", "Y", "X")),
        stringsAsFactors = FALSE
    )
    out <- rc_codebook(data, codebook, "variable", "from", "to",
                       factor_levels = "factor_levels")
    expect_equal(out, expected_out)
})

test_that("works with factors and NA values in x", {
    data[1, "x"] <- NA
    expected_out <- data.frame(
        PID = 1:6,
        x = factor(c(NA, "B", "C", "A", "B", "C"),
                   levels = c("A", "B", "C")),
        y = factor(c("X", "Y", "Z", "X", "Y", "Z"),
                   levels = c("X", "Y", "Z")),
        stringsAsFactors = FALSE
    )
    out <- rc_codebook(data, codebook, "variable", "from", "to",
                       factor_levels = "factor_levels")
    expect_equal(out, expected_out)
})

test_that("works with factors matching NA values in 'to' and 'factor_levels'", {
    codebook <- data.frame(variable = c("x", "x", "x", "y", "y", "y"),
                     from = c("1", "2", "3", "1", "2", "3"),
                     to = c(NA, "B", "C", "X", "Y", "Z"),
                     factor_levels = c(NA, 2, 3, 1, 2, 3),
                     stringsAsFactors = FALSE)
    expected_out <- data.frame(
        PID = 1:6,
        x = factor(c(NA, "B", "C", NA, "B", "C"),
                   levels = c("B", "C")),
        y = factor(c("X", "Y", "Z", "X", "Y", "Z"),
                   levels = c("X", "Y", "Z")),
        stringsAsFactors = FALSE
    )
    out <- rc_codebook(data, codebook, "variable", "from", "to",
                       factor_levels = "factor_levels")
    expect_equal(out, expected_out)
})

# ==========================================================
# Errors and warnings
# ==========================================================

codebook <- data.frame(variable = c("x", "x", "y", "y"),
                 from = c("0", "1", "0", "1"),
                 to = c("no", "yes", "no", "yes"),
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

test_that("identifies when there are unaccounted values in data", {
    dat <- data
    dat[1, "x"] <- "2"
    exo <- expected_out
    exo[1, "x"] <- "2"
    expect_equal(
        suppressWarnings(
            rc_codebook(dat, codebook, "variable", "from", "to")
        ),
        exo
    )
    expect_warning(
        rc_codebook(dat, codebook, "variable", "from", "to")
        )

})

test_that("identifies variables in codebook that are not contained in data", {

    codebook_2 <- rbind(codebook, c("z", "0", "no"))
    expect_equal(
        suppressWarnings(
            rc_codebook(data, codebook_2, "variable", "from", "to")
        ),
        expected_out
    )
    expect_warning(
        rc_codebook(data, codebook_2, "variable", "from", "to")
    )

})

