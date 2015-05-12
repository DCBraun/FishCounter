#' A function that plots the peak signal size (PSS) by date and time per hour of Logie counter data
#'
#' This function plots the PSS by day and time for Logie counter data
#' @param dataset This is the dataset used to create the plots.
#' @param first_day This is the first day of the dataset you want to use. This parameter needs to be specified in julian day format. Defaults to the first day in the dataset
#' @param last_day This is the last day of the dataset you want to use. This parameter needs to be specified in julian day format. Defaults to the last day in the dataset.
#' @param low_thresh is the counters lower threshold PSS value. Defaults to 0.
#' @param up_thresh is the counters upper threshold PSS value. Defaults to 130.
#' @param ch is the channel to be plotted. Defaults to all channels.
#' @keywords Events
#' @export

plot_pss_date <- function(dataset, 
                          first_day = NULL, 
                          last_day = NULL, 
                          low_thresh = NULL, 
                          up_thresh = NULL,
                          ch = NULL) {
  
  if(is.null(low_thresh)) {
    low_thresh <- 0
  }
  if(is.null(up_thresh)) {
    up_thresh <- 130
  }
  dataset$jday <- strptime(dataset$date, '%Y-%m-%d')$yday
  if(is.null(first_day)) {
    first_day <- min(dataset$jday, na.rm = TRUE)
  }
  if(is.null(last_day)) {
    last_day <- max(dataset$jday, na.rm = TRUE)
  }
  if(is.null(ch)) {
    ch <- seq(min(dataset$channel, na.rm = TRUE), max(dataset$channel, na.rm = TRUE), 1)
  }
  dataset           <- subset(dataset, channel %in% ch)
  dataset           <- dplyr::filter_(dataset, ~jday >= first_day, ~jday <= last_day)
  dataset$jday      <- NULL
  dataset$date_alt  <- NULL
  dataset$date_time <- strptime(paste(dataset$date, dataset$time, sep = " "), 
                                "%Y-%m-%d %H:%M:%S")
  
  dataset$date_time <- as.POSIXct(round(dataset$date_time, "hours"))
  
  par(mfrow = c(1, 1), 
      mar = c(2, 2, 2, 2), 
      oma = c(2, 2, 2, 2))
  
  plot(signal ~ date_time, data = dplyr::filter_(dataset, ~description == "U"), 
       col = "#00000010", 
       pch = 19, 
       cex = 1.5, 
       axes = FALSE, 
       las = 1, 
       xlab = "", 
       ylab = "", 
       ylim = c(low_thresh, up_thresh))
  
  par(new=TRUE)
  
  mean_signal <- plyr::ddply(dplyr::filter_(dataset, ~description == "U"), c("date"), 
                       summarize, mean_signal = mean(signal))
  
  mean_signal$date_alt <- as.POSIXct(strptime(mean_signal$date, "%Y-%m-%d"))
  
  plot(mean_signal ~ date_alt, data = mean_signal, typ = "p", col = "red", pch = 19, 
       cex = 1.5, axes = FALSE, las = 1, xlab = "", ylab = "", 
       ylim = c(low_thresh, up_thresh))
  
  r <- as.POSIXct(range(mean_signal$date_alt))
  
  axis.POSIXct(1, at = seq(r[1], r[2], by = "day"), format = "%b %d", cex.axis = 0.85, 
               col = "grey60")
  
  axis(2, las = 1, col = "grey60")
  
  box(col = "grey60")
  
  mtext("Peak signal", side = 2, line = 2.5, outer = FALSE, cex = 1.5)
  
  mtext("Date", side = 1, line = 3, outer = FALSE, cex = 1.5)
}