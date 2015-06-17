#' bind_signal_data
#' A function to bind and process Logie signal data
#' This function allows you to bind together mulitple signal data files, remove errors and produce a master datafile.
#' @param path_to_folder This is the file path for the folder that contains all data files for processing.
#' @param site Name of the study river.
#' @param year Year of counter operation.
#' @param max_signal The maximum signal size.
#' @keywords Logie
#' @export

bind_signal_data<-function(path_to_folder, site, year, max_signal, rows_rm=NULL){
  
  #"\\.txt$" tells r that the files are text files.  
  signal_paths <- dir(path_to_folder, pattern = "\\.txt$", full.names = TRUE)
  
  names(signal_paths) <- basename(signal_paths)
  
  if(missing(site)) {
    site <- ""
  }
  if(missing(year)) {
    year <- ""
  }
  
  signal_data1 <- ldply(signal_paths, 
                      read.table, 
                      header=FALSE, 
                      sep="", 
                      fill=TRUE, 
                      stringsAsFactors=FALSE)
  
  signal_data2 <- subset(signal_data1[, c(1:8)], V1=="S")[, -2]
  
  signal_data3 <- droplevels(signal_data2)
  
  colnames(signal_data3) <- c("file", 
                            "date", 
                            "time", 
                            "X", 
                            "channel", 
                            "description", 
                            "signal")
  
  signal_data4 <- data.frame("file"=signal_data3$file,
                           "date.time"=as.POSIXlt(strptime(paste(signal_data3$date, signal_data3$time, sep="-"), format='%d/%m/%y-%H:%M:%S')),
                           "date"=as.character(as.POSIXlt(strptime(signal_data3$date, format="%d/%m/%y"))),
                           "time"=as.character(signal_data3$time),
                           "X"=as.numeric(signal_data3$X),
                           "channel"=as.numeric(signal_data3$channel),
                           "description"=signal_data3$description,
                           "signal"=as.numeric(signal_data3$signal))
  
  signal_data5 <- signal_data4[!duplicated(signal_data4[, c(2, 6)]), ]
  
  signal_data <- signal_data5[signal_data5$signal <=max_signal, ]
  row_rm5     <- signal_data5[signal_data5$signal > max_signal, ]
  
  signal_data <- signal_data[order(signal_data$time), ]
  
  #try changing the encoding when exporting. Look at what the encoding is when using a PC. Load the graphics file onto Jan's computer.
  write.csv(x=signal_data[, -2], 
            file=paste(path_to_folder, 
                       site, 
                       year,
                       ".csv", 
                       sep=""),
                       row.names=FALSE)
  FuncOut <- list(wrong_pss=row_rm5)
  
  if(rows_rm=="TRUE"){
    return(FuncOut)  
  }
}