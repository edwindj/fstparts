context("repartition")

test_that("Split works", {
  parts <- fstparts(iris, dir=tempfile())
  reparts <- repartition_parts(parts, dir=tempfile(), chunksize = 30)
  expect_equal(length(reparts$parts), nrow(iris)/30)
})


test_that("Unequal split works", {
  parts <- fstparts(iris, dir=tempfile())
  reparts <- repartition_parts(parts, dir=tempfile(), chunksize = 149)
  expect_equal(length(reparts$parts), 2)
  expect_equal(reparts$parts[[1]]$size, 149)
  expect_equal(reparts$parts[[2]]$size, 1)
})

test_that("Combine works", {
  parts <- fstparts(iris[1:5,], dir=tempfile())
  parts <- append_part(parts, iris[6:12,])
  
  reparts <- repartition_parts(parts, dir=tempfile(), chunksize = 12)
  expect_equal(length(reparts$parts), 1)
  p1 <- read_part(reparts, 1)
  expect_equal(p1, iris[1:12,])
  
  reparts <- repartition_parts(parts, dir=tempfile(), chunksize = 6)
  expect_equal(length(reparts$parts), 2)
  p1 <- read_part(reparts, 1)
  expect_equal(p1, iris[1:6,])
  
  p2 <- read_part(reparts, 2)
  e2 <- iris[7:12,]
  rownames(e2) <- NULL
  
  expect_equal(p2, e2)
})

