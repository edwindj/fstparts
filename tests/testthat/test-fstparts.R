context("fstparts")

create_parts <- function(){
  fstparts( iris
          , dir=tempfile()
          , overwrite = T
  )
}

test_that("fstparts works", {
  l <- create_parts()
  expect_equal(l$name, "iris")
  expect_equal(names(l$columns), names(iris))
  expect_equivalent(l$columns, sapply(iris, class))
})

test_that("open_parts works",{
  l <- create_parts()
  m <- open_parts(l$dir)
  expect_equal(m$name, l$name)
  expect_equal(m$columns, l$columns)
  expect_equal(m$parts, l$parts)
  expect_equal(m$dir, l$dir)
  expect_equal(m$chunk_size, l$chunk_size)
  expect_equal(m$compress, l$compress)
})
