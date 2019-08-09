PARTNAME <- "%s_%04i.fst"

#' fst
#' 
#' @param data data to be added to parts
#' @param dir directory where to create
#' @param name name of data file
#' @param overwrite `logical`: should a previous directory be removed?
#' @param compress should the data be compressed (passed through to [fst::fst.write()])
#' @export
fstparts <- function( data
                    , dir       = NULL
                    , name      = deparse(substitute(data))
                    , overwrite = FALSE
 #                   , chunksize = 1e4L
                    , compress  = 0
                    , ...
                    ){
  if (is.null(dir)){
    stop("please specify the dir parameter")
  }
  if (is.null(data) && is.character(dir)){
    return(open_parts(dir))
  }
  
  path <- file.path(dir, sprintf(PARTNAME, name , 1))
  if (dir.exists(dir)){
    if (!overwrite){
      stop("Directory '", dir,"' already exists. Use overwrite=TRUE to force saving.", call. = FALSE)
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
  
  parts <- structure(
    list( dir        = dir
        , name       = name
        , columns    = columns
        , parts      = list(part)
#        , chunksize = chunksize
#        , compress   = compress
        )
    , class="fstparts"
  )
  write_index(parts)
  parts
}

#' @export
create_parts <- function(data, dir, ...){
  fstparts(data, dir, ...)
}

#' @export
dim.fstparts <- function(x){
  rowsize <- sum(sapply(x$parts, function(part){part$size}))
  c(rowsize, length(x$columns))
}

#' Open a fstparts file
#' 
#' @export
open_parts <- function(dir){
  path <- file.path(dir, "index.yml")
  if (!file.exists(path)){
    stop("No '", path, "' found.")
  }
  
  index <- yaml::yaml.load_file(path)
  
  index$dir <- dirname(path)
  structure(index, class="fstparts")
}

write_index <- function(parts){
  index <- parts
  index$dir <- NULL
  path <- file.path(parts$dir, "index.yml")
  writeLines(yaml::as.yaml(index), path)
  invisible(parts)
}

#' @export
is.fstparts <- function(x){
  inherits(x, "fstparts")
}

#' @export
is.parts <- function(x){
  inherits(x, "fstparts")
}

#fst::fst.metadata("tests/testthat/iris/iris_0001.fst")
