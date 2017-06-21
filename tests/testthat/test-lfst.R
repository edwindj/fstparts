context("lfst")

test_that("lfst works", {
  l <- lfst( iris
       #, dir=tempfile()
       , overwrite = T
       )
  expect_equal(l$name, "iris")
  expect_equal(names(l$columns), names(iris))
  expect_equivalent(l$columns, sapply(iris, class))
})

test_that("read_index works",{
  l <- lfst(iris, overwrite=T)
  m <- read_index(l$dir)
  expect_equal(m$name, l$name)
  expect_equal(m$columns, l$columns)
  expect_equal(m$parts, l$parts)
  expect_equal(m$dir, l$dir)
  expect_equal(m$chunk_size, l$chunk_size)
  expect_equal(m$compress, l$compress)
})
