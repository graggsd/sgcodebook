context("rc_factor")

# Basic functionality ---------------------------------------------------------
test_that("basic function without level index intact", {
    expect_equal(rc_factor(x = letters, from = letters, to = LETTERS),
                 factor(LETTERS))
    expect_equal(rc_factor(x = letters, from = letters, to = rep(LETTERS[1], length(letters))),
                 factor(rep(LETTERS[1], length(letters)), levels = LETTERS[1]))
    expect_equal(rc_factor(x = rep(letters[1], length(letters)),
                           from = letters,
                           to = LETTERS),
                 factor(rep(LETTERS[1], length(letters)), levels = LETTERS))
})

test_that("basic function using level index intact", {
    expect_equal(rc_factor(x = letters, from = letters, to = LETTERS, level_idx = 1:length(letters)),
                 factor(LETTERS))
    expect_equal(rc_factor(x = letters, from = letters, to = LETTERS, level_idx = rev(1:length(letters))),
                 factor(LETTERS, levels = rev(LETTERS)))
})

test_that("works when combining values in output", {
    expect_equal(
        rc_factor(x = c("a", "b", "c"), from = c("a", "b", "c"), to = c("A", "B", "B")),
        factor(c("A", "B", "B"), levels = c("A", "B"))
    )

    expect_equal(
        rc_factor(x = c("a", "b", "c"), from = c("a", "b", "c"),
                  to = c("A", "B", "B"), level_idx = c(1, 2, 2)),
        factor(c("A", "B", "B"), levels = c("A", "B"))
    )
})

test_that("works when some values in to are not in x", {
    expect_equal(
        rc_factor(x = c("b", "b", "c"), from = c("a", "b", "c"), to = c("A", "B", "C")),
        factor(c("B", "B", "C"), levels = c("A", "B", "C"))
    )
})

test_that("works when when NA values are contained in x", {
    expect_equal(
        rc_factor(x = c(NA, "b", "c"), from = c("a", "b", "c"), to = c("A", "B", "C")),
        factor(c(NA, "B", "C"), levels = c("A", "B", "C"))
    )
})


test_that("works when NA values that are paired in 'to' and 'level_idx'", {
    expect_equal(
        rc_factor(x = c("a", "b", "c"), from = c("a", "b", "c"),
                  to = c(NA, "B", "C"), level_idx = c(NA, 2, 1)),
        factor(c(NA, "B", "C"), levels = c("C", "B"))
    )

    expect_equal(
        rc_factor(x = c("a", "b", "c"), from = c("a", "b", "c"),
                  to = c(NA, NA, "C"), level_idx = c(NA, NA, 1)),
        factor(c(NA, NA, "C"), levels = c("C"))
    )
})

test_that("coerces characters appropriately", {
    expect_equal(rc_factor(x = letters,
                           from = letters,
                           to = LETTERS,
                           level_idx = 1:length(letters)),
                 rc_factor(x = letters,
                           from = letters,
                           to = LETTERS,
                           level_idx = as.character(1:length(letters))))
})

test_that("gives error with unexpected character coercion", {
    idx <- as.character(1:length(letters))
    idx[1] <- "1.A"
    expect_error(rc_factor(x = letters,
                           from = letters,
                           to = LETTERS,
                           level_idx = idx))
})
