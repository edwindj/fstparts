#' resave the fstparts with specified chunk size and compression
rewrite_parts <- function(x, path, chunksize=1e6, compress=0){
  if (!is.fstparts(x)){
    stop("x must be a fstparts object")
  }
  #TODO maybe allow a fstparts dir to be replaced? (first in tempdir, then mv?)
  
  parts <- 
    read_parts_chunked( x
                      , chunk_size = chunksize
                      , callback = function(dt, res){
                          if (is.null(res)){
                            res <- fstparts( dt
                                           , name=x$name
                                           , path = path
                                           , compress = compress
                                           , chunk_size = chunksize
                            )
                          } else {
                            res <- append_part(res, dt)
                          }
                          res
                      }
                    )
  parts
}

# export lfst to a single file or to a directory of csv
fstparts_to_csv <- function(x, path, one_file=TRUE, overwrite = FALSE, ...){
  # use fwrite to save the data.tables
  if (!isTRUE(one_file)){
    return(fstparts_to_csv_dir(x=x, path=path, overwrite = overwrite, ...))
  }
}

fstparts_to_csv_dir <- function( x
                           , path
                           , overwrite=FALSE
                           , create = TRUE
                           , ...
                           ){
  stop("Non implemented")
}


csv_to_fstparts <- function(path, chunksize = 1e6, compress=0){
  stop("Not implemented")
}