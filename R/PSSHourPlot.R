#' A function that plots the peak signal size (PSS) by hour of Logie counter data
#'
#' This function plots the PSS by hour for Logie counter data
#' @param dataset This is the dataset used to create the plots.
#' @param first_day This is the first day of the dataset you want to use. This parameter needs to be specified in julian day format. Defaults to the first day in the dataset
#' @param last_day This is the last day of the dataset you want to use. This parameter needs to be specified in julian day format. Defaults to the last day in the dataset.
#' @param low_thresh is the counters lower threshold PSS value. Defaults to 0.
#' @param up_thresh is the counters upper threshold PSS value. Defaults to 130.
#' @param ch is the channel to be plotted. Defaults to all channels.
#' @return Generates a plot of peak signal size by hour.
#' @keywords Events
#' @export

plot_pss_hour <- function(dataset, 
                          first_day = NULL, 
                          last_day  = NULL, 
                          low_thresh = NULL, 
                          up_thresh = NULL, 
                          ch = NULL) {
  
  if(is.null(low_thresh)) {
    low_thresh <- 0
  }
  if(is.null(up_thresh)) {
    up_thresh <- 130
  }
  
  dataset1     <- na.omit(dataset)
  dataset1$jday <- strptime(dataset1$date, '%Y-%m-%d')$yday
  if(is.null(first_day)) {
    first_day <- min(dataset1$jday, na.rm = TRUE)
  }
  if(is.null(last_day)) {
    last_day <- max(dataset1$jday, na.rm = TRUE)
  }
  
  if(is.null(ch)) {
    ch <- seq(min(dataset1$channel, na.rm = TRUE), max(dataset1$channel, na.rm = TRUE), 1)
  }
  
  dataset2          <- subset(dataset1, channel %in% ch)
  dataset3          <- dplyr::filter_(dataset2, ~jday >= first_day, ~jday <= last_day)
  dataset3$jday     <- NULL
  dataset3$date_alt <- NULL
  dataset3$hour     <- strptime(dataset3$time, format = "%H:%M:%S")
  dataset3$hour     <- as.POSIXct(round(dataset3$hour, "mins"))
  
  up_dataset1  		     <- dplyr::filter_(dataset3, ~description == "U")
  up_dataset1$count    <- 1
  up_dataset1$hour_24  <- substring(up_dataset1$hour, first=12, last=13)
  up_hour_count        <- plyr::ddply(up_dataset1, c("hour_24"), summarize, up_hour = sum(count))  
  
  #dev.new()
  m <- matrix(c(  0,0,0,0,
                  0,1,2,0,
                  0,1,2,0,
                  0,0,0,0), 4, 4) 
  
  layout(m, widths=c(0.75,2,2,0.25), heights=c(0.05,0.75,2,0.4))
  par(mar=c(0,0,0,0), oma=c(0,0,0,0), cex=1.25)
  
  plot(up_hour ~ hour_24, data = up_hour_count,
       col = "#00000070", pch = 19, cex = 1.5, axes = FALSE, las = 1, 
       xlab = "", ylab = "", type = "b")
  
  axis(2, las = 1, col = "grey60")
  box(col = "grey60")
  
  mtext("Number of Up Counts", 
        side = 2, 
        line = 4, 
        outer = FALSE, 
        cex = 1.5)
  
  plot(signal ~ hour, data = dataset3,
       col = "#00000010", pch = 19, cex = 1.5, axes = FALSE, las = 1, 
       xlab = "", ylab = "", ylim = c(low_thresh, up_thresh))
  
  axis.POSIXct(1, dataset3$hour, format = "%H:%M", cex.axis = 1, col = "grey60")
  
  axis(2, las = 1, col = "grey60")
  
  box(col = "grey60")
  
  mtext("Peak signal", 
        side = 2, 
        line = 4, 
        outer = FALSE, 
        cex = 1.5)
  
  mtext("Time of Day", 
        side = 1, 
        line = 3, 
        outer = FALSE, 
        cex = 1.5)
}