#' Append data to a fstparts directory
#' 
#' 
#' @export
append_part <- function(x, data){
  if (!inherits(x, "fstparts")){
    stop("not a valid lfst object")
  }
  
  nms <- names(x$columns)
  types <- sapply(data, class)[nms]
  same <- types == x$columns
  
  if (!all(same)){
    stop("type of columns differ")
    #TODO explain better what goes wrong
  }
  
  N <- length(x$parts) + 1
  
  part <- list(name = sprintf("part_%04i.fst", N), size=nrow(data))
  x$parts[[N]] <- part
  
  fst::write.fst(data, file.path(x$dir, part$name))
  
  write_index(x) # should we always write to index, or have a flush mechanism?
}