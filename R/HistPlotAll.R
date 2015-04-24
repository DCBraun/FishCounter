#' A function that plots histograms of Logie counter data
#'
#' This function plots historgrams of up, down and event counts for Logie counter data by channel
#' @param dataset This is the dataset used to create the histograms.
#' @param first_day This is the first day of the dataset you want to use. This parameter needs to be specified in julian day format. Defaults to the first day in the dataset
#' @param last_day This is the last day of the dataset you want to use. This parameter needs to be specified in julian day format. Defaults to the last day in the dataset.
#' @return Generates histograms of peak signal size for up counts, down counts and events for each counter channel.
#' @keywords Histogram
#' @export

hist_all_records <- function(dataset, 
                             first_day = NULL, 
                             last_day = NULL) {
  
  dataset$jday <- strptime(dataset$date, '%Y-%m-%d')$yday
  if(is.null(first_day)) {
    first_day <- min(dataset$jday, na.rm=TRUE)
  }
  if(is.null(last_day)) {
    last_day <- max(dataset$jday, na.rm=TRUE)
  }
  d1 <- dplyr::filter_(dataset, ~jday >= first_day, ~jday <= last_day)
  d <- dplyr::select(d1, channel, description, signal)
  ################
  par_ops <- list(mfrow = c(length(unique(d$channel)), 1), 
                  mar = c(4, 3, 1, 1), 
                  oma = c(2, 2, 0.5, 0), 
                  las = 1, 
                  xaxs = "i", 
                  yaxs = "i", 
                  cex = 1.5)
  
  dev.new()
  par(par_ops)
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
  
  print(no_events)
  ################
  dev.new()
  par(par_ops)
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
  print(no_up)
  ################
  dev.new()
  par(par_ops)
  
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
  print(no_down)
}