print.fstparts <- function(x, ...){
  cat( "Data parts '", x$name, "':\n"
    , "  rows: ", nrow(x)
    , ", parts: ",length(x$parts)
    , ", dir: '", x$dir, "'"
    , "\n"
    , sep= "")
  print(data.table::data.table(head(x)))
}