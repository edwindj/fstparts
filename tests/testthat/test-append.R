context("append")

test_that("append works", {
  l <- lfst(iris, name="append", overwrite = TRUE)
  l <- append_lfst(l, iris)
})
