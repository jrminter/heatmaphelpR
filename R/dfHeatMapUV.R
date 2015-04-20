#' dfHeatMapUV
#'
#' Plot a heat map from a data frame from UV measurements
#' using ggplot, passing limits.
#'
#' @param dat the data frame to be plotted
#'
#' @param myTitle a title for the plot
#'
#' @param lowLim the lower limit for the values (default 0)
#'
#' @param highLim the upper limit for the values (default 1)
#'
#' @param pt size for text annotation (default = 3)
#'
#' @return zp1 The ggplot2 plot
#'
#' @keywords keywords
#'
#' @export
dfHeatMapUV <- function(dat,
	                    myTitle,
                      lowLim=0,
                      highLim=1.5,
                      pt=3)
{
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(RColorBrewer)
  # Add a 'Position' column (don't just use row names...)
  Position <- c("1", "2", "3", "4", "5")
  work <- cbind(Position, dat)
  names(work) <- c("Position", "S1", "S2", "S3",
                   "S4", "S5", "S6", "S7")
  
  # we have to sort the columns
  work <- sortAscendingDataFrameCols(work, start=2)
 
  # we need to 'tidy' the data to get it into long form
  # tidyr supports the pipe (%>%) operator
  tidyDat <- work %>% 
    gather(Station, Density, S1:S7)
  
  # create a color pallette
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")),
                                space="Lab")
  # construct the plot with ggplot2 (more capable than ggvis...)
  # but does not yet support the %>% operator
  ret <- ggplot(tidyDat, aes(x = Station,
                             y = Position,
                             fill = Density))
  ret <- ret + geom_tile()
  ret <- ret + labs(title=myTitle)
  ret <- ret + scale_fill_gradientn(colours = myPalette(255),
                                    limits = c(lowLim, highLim))
  ret <- ret + geom_text(aes(fill=Density,
                             label=round(Density, 2)),
                         size=pt)
  ret <- ret + scale_x_discrete(expand = c(0, 0))
  ret <- ret + scale_y_discrete(expand = c(0, 0))
  ret <- ret + coord_equal()
  ret <- ret + theme_bw()
  # Set title to twice the base font size
  # ret <- ret + theme(plot.title = element_text(size = rel(1.25)))
  # ret <- ret + theme(axis.title = element_text(size = rel(1.15)))
  # ret <- ret + theme(axis.text = element_text(size = rel(1.10)))
  
  # we're out of here...
  return(ret)
}
