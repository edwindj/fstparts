#' read the fst parts in chunks
#' Note that the chunks may have a different size as the parts.
read_parts_chunked <- function(x, chunksize= 1e6L, callback, ...){
  # TODO check callback
  if (!is.fstparts(x)){
    stop("x should be a fstparts object", call. = F)
  }
  
  skip <- 0
  todo <- x$parts
  cb_result <- NULL
  
  while(length(todo)){
    res <- list()
    size <- chunksize
    while(size > 0 && length(todo)){
      part <- todo[[1]]
      
      if (!is.null(part$skip)){
        skip <- part$skip
      }
      path <- file.path(x$dir, part$name)
      from <- skip + 1
      to <- min(part$size, skip + size)
      
      res[[length(res) + 1]] <- fst::read.fst(path, from=from, to=to)
      size <- size - (to - skip)
      if (size || to == part$size){
        todo <- todo[-1] # remove the part
      } else
        todo[[1]]$skip <- to
    }
    dt <- data.table::rbindlist(res)
    cb_result <- callback(dt, cb_result)
  }
  invisible(cb_result)
}
