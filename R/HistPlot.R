#' A function that plots histograms of Logie counter data
#'
#' This function plots historgrams of up, down and event counts for Logie counter data by channel
#' @param dataset This is the dataset used to create the histograms.
#' @param day_one This is the first day of the dataset you want to use. This parameter needs to be specified in julian day format.
#' @param site Name of the study river.
#' @param year Year of counter operation.
#' @keywords Histogram
#' @export

hist_records <- function(dataset, direction, day_one=NULL, site=NULL, year=NULL) {
  record_type <- direction
  if(direction=="E"){
    direction_lab <- "EVENTS"
  }
  if(direction=="U"){
    direction_lab <- "UP"
  }
  if(direction=="D"){
    direction_lab <- "DOWN"
  }
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
  
  dev.new()
  par(mfrow = c(length(unique(d$channel)), 1), 
      mar = c(4, 3, 1, 1), 
      oma = c(2, 2, 0.5, 0), 
      las = 1, 
      xaxs = "i", 
      yaxs = "i", 
      cex = 1.5))

  records <- plyr::ddply(filter_(d, ~description == record_type), c("channel"), function(x) {
    hist(x$signal, breaks = seq(0, 130, 5), xlim = c(0, 130), main = "", ylab = "", 
         xlab = paste("Channel ", x$channel[1], sep = ""), col = "grey60")
    records <- length(x$signal)
    data.frame(records)
  }
  )
  
  mtext(paste("Frequency of ", direction_lab, " signal sizes", 
        side = 2, 
        outer = TRUE, 
        las = 0,
        cex = 1.5)
  
  print(records)