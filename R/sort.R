# implement a k-merge sort for multiple fst files

sort_parts <- function(x, keys, inline=TRUE){
  # first sort each fst file using data.table
  # using setkey, other option is using "order" but that is slower, but more flexible
  # merge sort of the results
  
}

setkey_parts <- function(parts, cols){
}

# library(data.table)
# dt <- data.table(iris)
# setkey(dt, Sepal.Length, Species)
# data.table::key(dt)
# View(dt)
