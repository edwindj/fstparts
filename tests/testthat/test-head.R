context("head")

test_that("head works", {
  parts <- fstparts(iris, dir=tempfile())
  expect_equal(head(parts), head(iris))
})
