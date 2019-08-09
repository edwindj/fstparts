#' Append data to a data parts directory
#'
#' Apppend data to a data parts directory
#' @param x existing parts object. 
#' @param data data.frame with data with same structure as `x`.
#' @return updated parts object.
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
  
  part <- list(name = sprintf(PARTNAME, x$name, N), size=nrow(data))
  x$parts[[N]] <- part
  
  fst::write.fst(data[, nms], file.path(x$dir, part$name))
  
  write_index(x) # should we always write to index, or have a flush mechanism?
}
