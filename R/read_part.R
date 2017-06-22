#' @export
read_part <- function(x, n = 1, ...){
  if (!is.fstparts(x)){
    stop("x must be a fstparts object")
  }
  if (n > length(x$parts) || n < 1){
    stop("the number of parts is maximally ", length(x$parts))
  }
  fst::read.fst( path = file.path(x$dir, x$parts[[n]]$name)
               , ...
               )
}
# 
# x <- fstparts(iris, dir=tempfile())
# x
# n <- 1
# 
# read_part(x)