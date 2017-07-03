#' @export
#' @importFrom utils tail
tail.fstparts <- function(x, n = 6L, ...){
  skip("Not working on travis...")
  N <- nrow(x)
  n <- min(n, N)
  
  part <- x$parts[[length(x$parts)]]
  path <- file.path(x$dir, part$name)
  from <- 1 + max(part$size - n, 0)
  dt <-fst::read.fst(path, from = from, to=part$size) 
  rownames(dt) <- seq(N-n+1, N)
  tail(dt)
}
