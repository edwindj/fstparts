context("read_part")

test_that("read_part works", {
  d1 <- iris[1:15,]
  d2 <- iris[16:30,]
  
  # otherwise we have an issue with row name comparison
  rownames(d1) <- NULL
  rownames(d2) <- NULL
  
  parts <- fstparts(d1, dir=tempfile())
  parts <- append_part(parts, d2)
  expect_equal(read_part(parts, 1), d1)
  expect_equal(read_part(parts, 2), d2)
})
