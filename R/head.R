#' @export
#' @importFrom utils head
head.fstparts <- function(x, n=6L, ...){
  part <- x$parts[[1]]
  path <- file.path(x$dir, part$name)
  to <- min(part$size, n)
  head(fst::read.fst(path, from =1, to=to))
}

#head(x)