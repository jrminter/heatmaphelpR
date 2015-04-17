#' dfHeatMap
#'
#' Plot a heat map from a data frame using ggplot, passing limits
#'
#' @param dat the data frame to be plotted
#'
#' @param dat myTitle a title for the plot
#'
#' @param lowLim the lower limit for the values
#'
#' @param highLim the upperr limit for the values
#'
#' @param dat Input data frame
#'
#' @return zp1 The ggplot2 plot
#'
#' @keywords keywords
#'
#' @export
dfHeatMap <- function(dat, myTitle, lowLim=0, highLim=1)
{
  library(ggplot2)
  library(reshape2)
  library(RColorBrewer)
  m <- as.matrix(dat)
  longData <- melt(m)
  colnames(longData) <- c("Position", "Station", "value")
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")),
                                space="Lab")
  zp1 <- ggplot(longData, aes(x = Station, y = Position, fill = value))
  zp1 <- zp1 + geom_tile()
  zp1 <- zp1 + labs(title=myTitle)
  zp1 <- zp1 + scale_fill_gradientn(colours = myPalette(255),
                                    limits = c(lowLim, highLim))
  zp1 <- zp1 + geom_text(aes(fill=value, label=round(value, 2)))
  zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
  zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
  zp1 <- zp1 + coord_equal()
  zp1 <- zp1 + theme_bw()
  return(zp1)
}