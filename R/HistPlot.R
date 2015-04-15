#' A function that plots histograms of Logie counter data
#'
#' This function plots historgrams of up, down and event counts for Logie counter data by channel
#' @param dataset This is the dataset used to create the histograms.
#' @param day_one This is the first day of the dataset you want to use. This parameter needs to be specified in julian day format.
#' @param site Name of the study river.
#' @param year Year of counter operation.
#' @keywords Histogram
#' @export

hist_records <- function(dataset, day_one=NULL, site=NULL, year=NULL) {
  
  dataset$jday <- strptime(dataset$date, '%Y-%m-%d')$yday
  if(is.null(day_one)) {
    day_one <- min(dataset$jday)
  }
  if(is.null(site)) {
    site <- as.character(dataset$site[1])
  }
   if(is.null(year)) {
     year <- substring(as.character(dataset$date[1]), first = 1, last = 4)
  }
  
  d1 <- dplyr::filter_(dataset, ~jday >= day_one)
  d <- dplyr::select(d1, channel, description, signal)
  
  # get rid of pdf function.
  #pdf(paste(getwd(),"/", site, year, "EventsbyChannel.pdf", sep = ""),
  #    height = 10,
  #    width = 10)
  par(mfrow = c(length(unique(d$channel)), 1), 
      mar = c(4, 3, 1, 1), 
      oma = c(2, 2, 0.5, 0), 
      las = 1, 
      xaxs = "i", 
      yaxs = "i",
      cex = 1.5,
      ask=TRUE)
  readline(prompt = "Pause. Press <Enter> to continue...")
  
  no_events <- plyr::ddply(filter_(d, ~description == "E"), c("channel"), function(x) {
    hist(x$signal, breaks = seq(0, 130, 5), xlim = c(0, 130), main = "", ylab = "", 
         xlab = paste("Channel ", x$channel[1], sep = ""), col = "grey60")
    no_events <- length(x$signal)
    data.frame(no_events)
  }
  )
  
  mtext("Frequency of EVENT signal sizes", 
        side = 2, 
        outer = TRUE, 
        las = 0,
        cex = 1.5)
  
  #dev.off()
  
  print(no_events)
  
  #pdf(paste(getwd(),"/", site, year, "UpsbyChannel.pdf", sep = ""),
  #    height = 10,
  #    width = 10)
  par(mfrow = c(length(unique(d$channel)), 1), 
      mar = c(4, 3, 1, 1), 
      oma = c(2, 2, 0.5, 0), 
      las = 1, 
      xaxs = "i", 
      yaxs = "i", 
      cex = 1.5)
  
  no_up <- plyr::ddply(filter_(d, ~description == "U"), c("channel"), function(x) {
    hist(x$signal, breaks = seq(0, 130, 5), xlim = c(0, 130), main = "", ylab = "", 
         xlab = paste("Channel ", x$channel[1], sep = ""), col = "grey60")
    no_up <- length(x$signal)
    data.frame(no_up)
  }
  )
  
  mtext("Frequency of UP signal sizes", 
        side = 2, 
        outer = TRUE, 
        las = 0,
        cex = 1.5)
  #dev.off()
  print(no_up)
  
  #pdf(paste(getwd(),"/", site, year, "DownsbyChannel.pdf", sep = ""),
  #    height = 10, 
  #    width = 10)
  par(mfrow = c(length(unique(d$channel)), 1), 
      mar = c(4, 3, 1, 1), 
      oma = c(2, 2, 0.5, 0), 
      las = 1, 
      xaxs = "i", 
      yaxs = "i",
      cex = 1.5)
  
  no_down <- plyr::ddply(filter_(d, ~description == "D"), c("channel"), function(x) {
    hist(x$signal, breaks = seq(0, 130, 5), xlim = c(0, 130), main = "", ylab = "", 
         xlab = paste("Channel ", x$channel[1], sep = ""), col = "grey60")
    no_down <- length(x$signal)
    data.frame(no_down)
  }
  )
  
  mtext("Frequency of DOWN signal sizes", 
        side = 2, 
        outer = TRUE, 
        las = 0,
        cex = 1.5)
  #dev.off()
  print(no_down)
}
