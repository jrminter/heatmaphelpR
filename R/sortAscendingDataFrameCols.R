#' sortAscendingDataFrameCols
#'
#' Sort each column of a data frame into ascending order. Useful for
#' a few features can be flipped.
#'
#' @param dat Input data frame
#'
#' @param start starting column (default = 1)
#'
#' @return The sorted data frame
#' @keywords keywords
#'
#' @export
sortAscendingDataFrameCols <- function(dat, start=1){
  # sort each column of a data frame in ascending order
  nc <- ncol(dat)
  for (i in start:nc){
    x <- dat[, i]
    y <- x[order(x)]
    dat[, i] <- y
  }
  return(dat)
}
