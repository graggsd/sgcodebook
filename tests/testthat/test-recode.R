context("recode")


# Basic functionality ---------------------------------------------------------
test_that("for character vectors", {
    expect_equal(recode(x = letters, from = letters, to = LETTERS),
                 LETTERS)
    expect_equal(recode(x = letters[1:10], from = letters, to = LETTERS),
                 LETTERS[1:10])
})

test_that("reassigns all values in 'from' to a single value in 'to' when 'to' contains a single value", {
    expect_equal(recode(x = letters, from = letters, to = LETTERS[1]),
                 rep(LETTERS[1], length(letters)))
})

test_that("can reassign multiple values in 'from' to a single value of 'to'", {
    expect_equal(recode(x = c("a", "b"), from = c("a", "b"), to = c("1", "1")),
                 c("1", "1"))
})

test_that("works with 'x' of length 1", {
    expect_equal(recode(x = letters[1], from = letters, to = LETTERS),
                 LETTERS[1])
})

test_that("works with 'to' and 'from' of length 1", {
    expect_equal(recode(x = c("a", "x"), from = "a", to = "b"),
                 c("b", "x"))
})

test_that("works with coercion", {
    expect_equal(recode(x = rep(1:10),
                        from = as.character(1:10),
                        to = letters[1:10]),
                 letters[1:10])
})

# NA value handling ------------------------------------------------------------
test_that("handles NA values in 'x'", {
    x <- c("a", NA, "b", "b")
    from <- c("a", "b")
    to <- c("1", "2")
    out <- c("1", NA, "2", "2")
    expect_equal(recode(x, from, to), out)
})

test_that("handles NA values in 'to'", {
    x <- c("a", "a", "b", "b")
    from <- c("a", "b")
    to <- c("1", NA)
    out <- c("1", "1", NA, NA)
    expect_equal(recode(x, from, to), out)
})
