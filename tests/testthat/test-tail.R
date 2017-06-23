context("tail")

test_that("tail works", {
  dat <- iris
  rownames(dat) <- NULL
  parts <- fstparts(dat, dir=tempfile())
  t_o <- tail(parts)
  t_e <- tail(dat)
  rownames(t_o) <- rownames(t_e) <- NULL 
  expect_equal(t_o, t_e)
})
