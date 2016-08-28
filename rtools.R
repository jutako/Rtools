

#' Circularly shift vector elemens
#' To e.g. change the position of a peak within a series
#' positive n shifts things to the left, negative to the right
#' 
#' From: http://stackoverflow.com/questions/30542128/circular-shifting-arrays-in-r-by-distance-n
shifter <- function(x, n) {
  if (n == 0) x else c(tail(x, -n), head(x, n))
}


#' Transform a matrix into a ggplottable data.frame
make.ggplot.df <- function(dmat){
  pd <- as.data.frame(dmat)
  pd$row <- 1:nrow(pd)
  pd <- reshape2::melt(pd, id.vars = c('row'))
  pd
}