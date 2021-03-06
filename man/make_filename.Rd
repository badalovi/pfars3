% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{make_filename}
\alias{make_filename}
\title{Create fars dataset name}
\usage{
make_filename(year)
}
\arguments{
\item{year}{A number.}
}
\value{
This function returns file name concatinated with the year input.
If input is integer or a class which can be coerced to integer, it easily
return expected output, otherwise filename would include NA.
}
\description{
This is a simple function to create names for datasets from US National
Highway Traffic Safety Administration's Fatality Analysis Reporting System
in the working directory to use them in the relevant functions.
You can customize year in the dataset name (using the \code{year} argument).
}
\examples{
?
make_filename(2013)
make_filename(2014)
}
