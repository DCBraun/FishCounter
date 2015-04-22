#' bind_counter_data
#' bind individual data files, clean up data, and return a master data file.
#' A function to bind and process Logie counter data
#' This function allows you to bind together mulitple counter data files, remove errors and produce a master datafile.
#' @param path_to_folder This is the file path for the folder that contains all data files for processing.
#' @param no_channels This is the number of counter channels that were operated.
#' @param site Name of the study river.
#' @param year Year of counter operation.
#' @param max_signal The maximum signal size.
#' @export


bind_counter_data <- function(path_to_folder=".", no_channels, site, year, max_signal, rows_rm=NULL) {
  #path_to_folder use current wd() if not specified. Make sure it works.  
  #add in metadata to the output file. e.g., number of channels, site name, etc.
  
  if(is.null(site)) {
    site <- ""
  }
  if(is.null(year)) {
    year <- ""
  }
  
  counter_paths <- dir(path_to_folder, full.names = TRUE)
  names(counter_paths) <- basename(counter_paths)
  
  counter_data1 <- plyr::ldply(counter_paths, 
                       read.table, 
                       header=FALSE, 
                       sep="", 
                       fill=TRUE, 
                       stringsAsFactors=FALSE)[, c(1:7)]
  
  #stringsAsFactors=FALSE is important because conversion 
  #of numeric factors to numeric can be problematic.
  
  colnames(counter_data1) <- c("file", 
                             "date", 
                             "time", 
                             "X", 
                             "channel", 
                             "description", 
                             "signal")
  
  counter_data2 <- counter_data1[counter_data1$description == "U" | 
                    counter_data1$description == "D" | 
                    counter_data1$description == "E", ]
  
  row_rm1        <- counter_data1[counter_data1$description != "U" & 
                    counter_data1$description != "D" & 
                    counter_data1$description != "E", ]
  
  #This removes erronious data or unwanted counter status data
  
  date.alt <- strptime(counter_data2$date, '%d/%m/%y')
  counter_data2$jday <- date.alt$yday
  counter_data3 <- subset(counter_data2, jday != "NA")#check to see if I need to convert to jday
  
  counter_data4 <- data.frame("file"=counter_data3$file, 
      "date.time"=as.character(as.POSIXlt(strptime(paste(counter_data3$date, 
                                                   counter_data3$time, sep="-"), 
                                                   format='%d/%m/%y-%H:%M:%S'))),
      "date"=as.character(as.POSIXlt(strptime(counter_data3$date, 
                                              format="%d/%m/%y"))),
      "time"=as.character(counter_data3$time),
      "X"=as.numeric(counter_data3$X),
      "channel"=as.numeric(counter_data3$channel),
      "description"=counter_data3$description,
      "signal"=as.numeric(counter_data3$signal))
  
  counter_data5 <- counter_data4[counter_data4$channel <= (no_channels), ]
  row_rm5       <- counter_data4[counter_data4$channel > (no_channels), ]
  # removes any errors in channel number
  
  counter_data6 <- counter_data5[!duplicated(counter_data5[, c(2, 6)]), ]
  # removes any duplicate data
  
  counter_data7 <- counter_data6[counter_data6$signal <= max_signal, ]
  row_rm7       <- counter_data6[counter_data6$signal > max_signal, ]
  
  # gets rid of levels that have been subseted out. 
  
  counter_data8 <- droplevels(counter_data7) 
  # gets rid of levels that have been subseted out. 
  
  counter_data9 <- counter_data8[order(counter_data8$date.time), ]
  counter_data <- data.frame("site" = site, counter_data9)
  # Now write a new text file with only the graphics data. 
  # The row names, column names and quotes must be removed. 
  
  write.csv(x=counter_data[, -3], 
            file=paste(path_to_folder,
                       site, 
                       year,
                       ".csv", 
                       sep=""), 
            row.names=FALSE)
  #invisible(counter_data[,-2])
  FuncOut <- list(wrong_description=row_rm1, wrong_channel=row_rm5, wrong_pss=row_rm7)
  
  if(rows_rm=="TRUE"){
    return(FuncOut)  
  }
}