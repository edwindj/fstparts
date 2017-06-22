#' @export
tail.fstparts <- function(x, n = 6L, ...){
  part <- x$parts[[length(x$parts)]]
  path <- file.path(x$dir, part$name)
  from <- 1 + max(part$size - n, 0)
  tail(fst::read.fst(path, from = from, to=part$size))
}
