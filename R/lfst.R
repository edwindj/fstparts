#' fst
#' @export
lfst <- function( data
                , name       = deparse(substitute(data))
                , dir        = name
                , overwrite  = FALSE
                , chunk_size = 1e4L
                , compress   = 0
                ){
  path <- file.path(dir, "part_0001.fst")
  if (dir.exists(dir)){
    if (!overwrite){
      stop("Use overwrite=TRUE", call. = FALSE)
    }
    unlink(dir, recursive = TRUE)
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  fst::write.fst(data, path=path, compress = compress)
  
  columns <- lapply(names(data), function(name){
    list(name=name, type=class(data[[name]]))
  })

  columns <- lapply(data, class)
  names(columns) <- names(data)
  
  part <- 
    list(name = basename(path), size = nrow(data))
  
  lfst <- structure(
    list( dir        = dir
        , name       = name
        , columns    = columns
        , parts      = list(part)
        , chunk_size = chunk_size
        , compress   = compress
        )
    , class="lfst"
  )
  write_index(lfst)
  lfst
}

read_index <- function(dir){
  path <- file.path(dir, "index.yml")
  index <- yaml::yaml.load_file(path)
  #TODO check structure
  index$dir <- dirname(path)
  structure(index, class="lfst")
}

write_index <- function(lfst){
  index <- lfst
  index$dir <- NULL
  path <- file.path(lfst$dir, "index.yml")
  writeLines(yaml::as.yaml(index), path)
  invisible(lfst)
}

#fst::fst.metadata("tests/testthat/iris/iris_0001.fst")
