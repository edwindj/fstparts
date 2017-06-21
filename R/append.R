# append a data set to the lfst.
# can be really simple by just saving the fst and updating the index
# or can be more sophisticated by append to last chunk. 

append_lfst <- function(x, data){
  if (!inherits(x, "lfst")){
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
  
  write_index(x)
  # check if data layout is compatible
  # write to directory
  # update index in lfst
  x
}