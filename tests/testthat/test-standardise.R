box::use(
    tt = testthat,
)

box::use(
    be = app / logic / backend,
)

no_missings <- 1:3

tt$test_that(
    tt$expect_equivalent(be$standardise(no_missings), c(-1, 0, 1)),
    desc = "standardise works correctly without missings"
)

tt$test_that(
    tt$expect_equivalent(be$standardise(c(NA, no_missings)), c(NA, -1, 0, 1)),
    desc = "standardise works correctly with missings"
)
