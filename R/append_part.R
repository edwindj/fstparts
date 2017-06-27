#' Append data to a fstparts directory
#'
#' Apppend data to a fstparts directory
#' @param x existing fstparts object. 
#' @param data data.frame with data with same structure as \code{x}.
#' @return updated fstparts object.
#' @export
append_part <- function(x, data){
  if (!inherits(x, "fstparts")){
    stop("not a valid fstparts object")
  }
  
  nms <- names(x$columns)
  types <- sapply(data, class)[nms]
  same <- types == x$columns
  
  if (!all(same)){
    stop("type of columns differ")
    #TODO explain better what goes wrong
  }
  
  N <- length(x$parts) + 1
  
  if (N >= 1e4) {
    stop("currently the maximum number of parts is 1e4. Please repartition with a larger chunksize")
  }
  
  part <- list(name = sprintf("part_%04i.fst", N), size=nrow(data))
  x$parts[[N]] <- part
  
  fst::write.fst(data, file.path(x$dir, part$name))
  
  write_index(x) # should we always write to index, or have a flush mechanism?
}