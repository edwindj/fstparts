context("append_part")

test_that("append_part works", {
  l <- fstparts(iris, name="append", overwrite = TRUE)
  l <- append_part(l, iris)
})
