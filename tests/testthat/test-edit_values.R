cb <- data.frame(id = c(1, 2),
                 variable = c("x", "y"),
                 from = c("a", "b"),
                 to = c("x", "y"),
                 stringsAsFactors = FALSE)

dat <- data.frame(id = c(1, 2, 3),
                  x = c("a", "a", "a"),
                  y = c("b", "b", "b"),
                  z = c("c", "c", "c"),
                  stringsAsFactors = FALSE)

expected <- data.frame(id = c(1, 2, 3),
                  x = c("x", "a", "a"),
                  y = c("b", "y", "b"),
                  z = c("c", "c", "c"),
                  stringsAsFactors = FALSE)

test_that("works as expected", {
    expect_equal(edit_values(dat, cb),
                 expected)
})

