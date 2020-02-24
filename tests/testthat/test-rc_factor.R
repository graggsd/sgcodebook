# Basic functionality ---------------------------------------------------------
test_that("rc_factor works without level index", {
    expect_equal(rc_factor(x = letters, from = letters, to = LETTERS),
                 factor(LETTERS))
    expect_equal(rc_factor(x = letters, from = letters, to = rep(LETTERS[1], length(letters))),
                 factor(rep(LETTERS[1], length(letters)), levels = LETTERS[1]))
    expect_equal(rc_factor(x = rep(letters[1], length(letters)),
                           from = letters,
                           to = LETTERS),
                 factor(rep(LETTERS[1], length(letters)), levels = LETTERS))
})

test_that("rc_factor works with level index", {
    expect_equal(rc_factor(x = letters, from = letters, to = LETTERS, level_idx = 1:length(letters)),
                 factor(LETTERS))
    expect_equal(rc_factor(x = letters, from = letters, to = LETTERS, level_idx = rev(1:length(letters))),
                 factor(LETTERS, levels = rev(LETTERS)))
})
