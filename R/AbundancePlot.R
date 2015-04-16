#' plot_abundance
#' A function to bind and process Logie signal data
#' This function allows you to bind together mulitple signal data files, remove errors and produce a master datafile.
#' @param dataset This is the object containing the data for plotting.
#' @param day_one This is the first day the data should start on.
#' @keywords Logie
#' @export

plot_abundance <- function(dataset, day_one) {
  
  dataset$jday <- strptime(dataset$date, '%Y-%m-%d')$yday
  if(is.null(day_one)) {
    day_one <- min(dataset$jday)
  }
  
  dataset           <- dplyr::filter_(dataset, ~jday>=day_one)
  dataset$jday      <- NULL
  dataset$date_alt  <- NULL
  dataset$date_time <- strptime(paste(dataset$date, dataset$time, sep=" "), 
                                "%Y-%m-%d %H:%M:%S")
  
  dataset$date_time <- as.POSIXct(round(dataset$date_time, "day"))
  
  updata <- data.frame(dplyr::filter_(dataset, ~description == "U"), count = 1) 
  updata$cummulative_count <- cumsum(updata$count)
  
  up <- plyr::ddply(updata, c("date_time"), summarize, 
              daily_count = sum(count), 
              count = max(cummulative_count))
  dev.new()
  par(mfrow = c(2, 1), 
      mar = c(0, 2, 0, 2), 
      oma = c(6, 2, 2, 1))
  
  plot(daily_count ~ date_time, data = up, 
       pch = 19, 
       cex = 1.5, 
       axes = FALSE, 
       las = 1, 
       xlab = "", 
       ylab = "",
       type = "b")
  
  mtext("Fish per day", side = 2, line = 2.5, outer = FALSE, cex = 1.5)  
  
  axis(2, las=1)
  box()
  
  plot(count ~ date_time, data = up, 
       ylim = c(0, max(up$count) * 1.1), 
       typ = "b", 
       xlab = "", 
       ylab = "", 		
       axes = FALSE, 
       lwd = 2, 
       pch = 19)
  
  mtext("Total no. of fish", side=2, line = 2.5, cex = 1.5, las = 0)
  mtext("Date", side=1, line=4,cex=1.5)
  
  r <- as.POSIXct(range(up$date_time))
  axis.POSIXct(1, at=seq(r[1], r[2], by="day"), format="%b %d", cex.axis=0.85)
  axis(2, las=1)
  box()
}