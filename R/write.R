# resave the lfst file with specified chunk size and compression

write_lfst <- function(lfst, path, chunksize=1e6, compress=0){
  
}

# export lfst to a single file or to a directory of csv
lfst_to_csv <- function(lfst, path, one_file=TRUE, overwrite = FALSE, ...){
  # use fwrite to save the data.tables
  if (!isTRUE(one_file)){
    return(lfst_to_csv_dir(lfst=lfst, path=path, overwrite = overwrite, ...))
  }
}

lfst_to_csv_dir <- function( lfst
                           , path
                           , overwrite=FALSE
                           , create = TRUE
                           , ...
                           ){
  stop("Non implemented")
}


csv_to_lfst <- function(path, chunksize = 1e6, compress=0){
  stop("Not implemented")
}