cb <- data.frame(id = c(1, 2),
                 notess = c("", ""),
                 stringsAsFactors = FALSE)

dat <- data.frame(id = c(1, 2, 3),
                  x = c("a", "a", "a"),
                  y = c("b", "b", "b"),
                  z = c("c", "c", "c"),
                  stringsAsFactors = FALSE)

expected <- dat[3, ]

test_that("works as expected", {
    expect_equal(exlude_obs(dat, cb),
                 expected)
})
