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
  
  
  up_hour1               <- data.frame(dplyr::filter_(d1, ~description == "U"), no = 1)
  up_hour1$date_time_alt <- as.character(as.POSIXct(strptime(paste(up_hour1$date, 
                                                                       up_hour1$time,sep = " "), '%Y-%m-%d %H')))
  up_hour1$date_alt      <- NULL
  up_hour1$jday          <- NULL
  
  up_hour_channel <- plyr::ddply(up_hour1, c("date_time_alt", "channel"), 
                                     summarize, no_ups = sum(no))
  
  up_hour_channel$date_time_alt <- as.POSIXct(strptime(up_hour_channel$date_time_alt, 
                                                           format = "%Y-%m-%d %H"))
  hour_channel <- merge(events_hour_channel, up_hour_channel, all  =TRUE)
  hour_channel[is.na(hour_channel)] <- 0
  
  dev.new()
  par(mfrow = c(length(unique(events_hour1$channel)), 1), 
      mar = c(4, 2, 0.5, 4), 
      oma = c(2, 4, 0.5, 2),
      las = 1,
      cex = 1.5)
  
  events_hour_ch <- plyr::ddply(hour_channel, c("channel"), function(x) {
    
  plot(x$no_events ~ as.POSIXct(x$date_time_alt), main = "", ylab = "", 
      ylim = c(0, max(hour_channel$no_events) * 1.05), 
      xlim = c(r[1], r[2]), 
      xlab = paste("Channel ", x$channel[1], sep = " "), 
      type = "l", 
      lwd = 2, 
      axes = FALSE)
  
  lines(x = c(r[1], r[2]), 
        y = c(mean(events_hour_channel$no_events), 
              mean(events_hour_channel$no_events)), 
        col = "red", 
        lty = 2, 
        lwd = 2)
  
  par(new=TRUE)
  
  plot(x$no_ups ~ as.POSIXct(x$date_time_alt), main = "", ylab = "", 
       ylim = c(0, max(hour_channel$no_ups) * 1.05), 
       xlim = c(r[1], r[2]), 
       xlab = "", 
       type = "l", 
       lwd = 1.5, 
       col = "#0000FF70",
       axes = FALSE)
    
    axis(2, cex = 1.5)
    axis(4, cex = 1.5)
    axis.POSIXct(1, at = seq(r[1], r[2], by = "day"), format = "%b %d", cex.axis = 0.9)
    
    box()
  
  if(x$channel[1] == 1){
    legend("topleft", max(events_hour_channel$no_events), 
           c("Mean events per hour", "Events per hour", "Up counts per hour"), 
           lwd = 3, 
           col = c("red", "black", "#0000FF70"),
           lty = c(2, 1, 1), 
           cex = 0.6)  
  }
    
    data.frame(date_time = x$date_time_alt, no_events = x$no_events)
  }
  )
  
  mtext("Number of Events per hour", 
        side = 2, 
        line = 2, 
        outer = TRUE, 
        las = 0, 
        cex = 1.5)
  
  mtext("Number Up Counts per hour", 
        side = 4, 
        line = 0, 
        outer = TRUE, 
        las = 0, 
        cex = 1.5)
  print(events_hour_ch)
}
