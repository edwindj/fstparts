#' resave the fstparts with specified chunk size and compression
#' 
repartition_parts <- function(x, dir, chunksize=1e6, compress=0){
  if (!is.fstparts(x)){
    stop("x must be a fstparts object")
  }
  #TODO maybe allow a fstparts dir to be replaced? (first in tempdir, then mv?)
  
  parts <- 
    read_parts_chunked( x
                      , chunksize = chunksize
                      , callback   = function(dt, res){
                          if (is.null(res)){
                            res <- fstparts( dt
                                           , name      = x$name
                                           , dir       = dir
                                           , compress  = compress
                                           , chunksize = chunksize
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
parts_to_csv <- function(x, path, one_file=TRUE, overwrite = FALSE, ...){
  if (!inherits(x, "fstparts")){
    stop("x should be a fstparts object")
  }
  if (!isTRUE(one_file)){
    return(parts_to_csv_dir(x=x, path=path, overwrite = overwrite, ...))
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  for (i in seq_along(x$parts)){
    dt <- read_part(x, i)
    data.table::fwrite( dt
                      , file   = path
                      , append = (i > 1)
                      , ...
                      )
  }
}

parts_to_csv_dir <- function( x
                            , path
                            , overwrite=FALSE
                            , create = TRUE
                            , ...
                            ){
  if (!inherits(x, "fstparts")){
    stop("x should be a fstparts object")
  }
  dir.create(path, recursive = TRUE)
  for (i in seq_along(x$parts)){
    dt <- read_part(x, i)
    fpath <- file.path(path, x$parts[[i]]$name)
    fpath <- sub("\\.fst", ".csv", fpath)
    data.table::fwrite(dt, file = fpath, append =  FALSE, ...)
  }
}


csv_to_parts <- function(path, chunksize = 1e6, compress=0){
  stop("Not implemented")
}