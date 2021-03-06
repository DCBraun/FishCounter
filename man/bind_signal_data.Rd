% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/SignalBind.R
\name{bind_signal_data}
\alias{bind_signal_data}
\title{bind_signal_data
A function to bind and process Logie signal data
This function allows you to bind together mulitple signal data files, remove errors and produce a master datafile.}
\usage{
bind_signal_data(path_to_folder, site, year, max_signal, rows_rm = NULL)
}
\arguments{
\item{path_to_folder}{This is the file path for the folder that contains all data files for processing.}

\item{site}{Name of the study river.}

\item{year}{Year of counter operation.}

\item{max_signal}{The maximum signal size.}
}
\description{
bind_signal_data
A function to bind and process Logie signal data
This function allows you to bind together mulitple signal data files, remove errors and produce a master datafile.
}
\keyword{Logie}

