#' A function that plots events per hour of Logie counter data
#'
#' This function plots the number of events per hour channel and day for Logie counter data by channel
#' @param data This is the dataset used to create the plots.
#' @param day_one This is the first day of the dataset you want to use. This parameter needs to be specified in julian day format.
#' @return Generates three .pdf files with histograms of peak signal size for up counts, down counts and events for each counter channel.
#' @keywords Events
#' @export

plot_events <- function(dataset, day_one=NULL) {

  dataset$jday <- strptime(dataset$date, '%Y-%m-%d')$yday
  if(is.null(day_one)) {
    day_one <- min(dataset$jday)
  }
  
  d1 <- dplyr::filter_(dataset, ~jday >= day_one)
  
  events_hour1               <- data.frame(dplyr::filter_(d1, ~description == "E"), no = 1)
  events_hour1$date_time_alt <- as.character(as.POSIXct(strptime(paste(events_hour1$date, 
                                  events_hour1$time,sep = " "), '%Y-%m-%d %H')))
  events_hour1$date_alt      <- NULL
  events_hour1$jday          <- NULL
  
  events_hour_channel <- plyr::ddply(events_hour1, c("date_time_alt", "channel"), 
                               summarize, no_events = sum(no))
  
  events_hour_channel$date_time_alt <- as.POSIXct(strptime(events_hour_channel$date_time_alt, 
                                                           format = "%Y-%m-%d %H"))
  
  r <- range(events_hour_channel$date_time_alt)
  
  dev.new()
  par(mfrow = c(length(unique(events_hour1$channel)), 1), 
      mar = c(4, 2, 0.5, 2), 
      oma = c(2, 4, 0.5, 2),
      las = 1,
      cex = 1.5)
  
  events_hour_ch <- plyr::ddply(events_hour_channel, c("channel"), function(xx) {
    
  plot(xx$no_events ~ as.POSIXct(xx$date_time_alt), main = "", ylab = "", 
      ylim = c(0, max(events_hour_channel$no_events) * 1.05), 
      xlim = c(r[1], r[2]), 
      xlab = paste("Channel ", xx$channel[1], sep = " "), 
      type = "l", 
      lwd = 2, 
      axes = FALSE)
    
    lines(x = c(r[1], r[2]), 
        y = c(mean(events_hour_channel$no_events), 
        mean(events_hour_channel$no_events)), 
        col = "red", 
        lty = 2)
    
    axis(2, cex = 1.5)
    
    axis.POSIXct(1, at = seq(r[1], r[2], by = "day"), format = "%b %d", cex.axis = 0.9)
    
    box()
    
    data.frame(date_time = xx$date_time_alt, no_events = xx$no_events)
  }
  )
  
  mtext("mean events per hour for all channels", 
        side = 2, 
        outer = TRUE, 
        col = "red", 
        line = -24, 
        at = 1, 
        las = 1,
        cex = 1.5)
  
  mtext("No. Events per hour", 
        side = 2, 
        line = 2, 
        outer = TRUE, 
        las = 0, 
        cex = 1.5)
  
  legend("topleft", max(events_hour_channel$no_events), 
         c("Mean events per hour"), 
         lwd=3, 
         col="red")  
  
  print(events_hour_ch)
}