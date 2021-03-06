context("append_part")

test_that("append_part works", {
  l <- fstparts(iris, dir=tempfile(), overwrite = TRUE)
  l <- append_part(l, iris)
  expect_equal(nrow(l), 2*nrow(iris))
})
