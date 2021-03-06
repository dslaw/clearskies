% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/clearskymodels.R
\name{RS}
\alias{RS}
\title{Robledo-Soler clear sky model}
\usage{
RS(dayofyear, year, tz, latitude, longitude, interval, ..., parameters = c(a =
  1159.24, b = 1.179, c = -0.0019))
}
\arguments{
\item{dayofyear}{The day of year to fit the model to. May be either a single
value or a vector.}

\item{year}{The year to fit the model to. The model is fit over both
dayofyear and year, with the shorter vector being recycled as normal.}

\item{tz}{UTC Offset. Ex: Eastern Standard Time = -5.}

\item{latitude}{Latitude at the location for which the model is to be fit.}

\item{longitude}{Longitude at the location for which the model is to be fit.}

\item{interval}{Number of minutes between clear sky points. Defaults to 1
(every minute). Must be an integer between 1 and 60, inclusive.}

\item{parameters}{Robledo-Soler model parameters. Named vector or list
containing values for a, b and c.}
}
\value{
Vector of fitted irradiance values for the given time period.
}
\description{
Fit Robledo-Soler (RS) clear sky model.
}
\keyword{internal}

