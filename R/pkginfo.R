## Documentation for clearskies package, data and C++ directives

#' clearskies: R package for fitting clear sky models and detecting clear
#' points.
#'
#' The clearskies package contains functions for fitting a number of clear sky
#' models and a function for running the clear sky detection algorithm.
#'
#' @section Clear Sky Models:
#' Clear sky models are fit through the clear_sky function, which provides a
#' convenient interface to all available clear sky models. Models can be fit by
#' passing either a dataframe, named list or named vector of date information to
#' x and a named object of location information to y, or by explicitly passing
#' the necessary date and location parameters to their respective arguments.
#' Parameters specific to the model type may be passed through a named object to
#' argument "parameters".
#'
#' @section Clear Sky Detection:
#' The clear sky detection algorithm can be run on measured irradiance and a
#' corresponding clear sky model using the clear_points function. clear_points
#' requires that the thresholds argument is ordered correctly, see ?clear_points
#' for details. Thresholds for 10-minute rolling windows can be found in data/.
#'
#' More information on the clear sky detection algorithm can found in:
#' Global Horizontal Irradiance Clear Sky Models: Implementation and Analysis,
#' Reno et al, 2012, pp. 28.
#'
#' @docType package
#' @name clearskies
NULL

#' Clear sky detection thresholds for 10-minute windows.
#'
#' Contains threshold values used for detecting clear points with a 10-minute
#' window.
#'
#' @format A named list with 5 components:
#' \describe{
#'      \item{Mean}{Vector of minimum and maximum mean values}
#'      \item{Max}{Vector of minimum and maximum max values}
#'      \item{Line.length}{Vector of minimum and maximum line length values}
#'      \item{Sigma}{Vector of minimum and maximum sigma values}
#'      \item{Deviation}{Vector of minimum and maximum slope deviation values}
#' }
#'
#' @section References:
#' Global Horizontal Irradiance Clear Sky Models: Implementation and Analysis,
#' Reno et al, 2012, pp. 34.
#'
"thresholds"

#' Location information for 10 sites in the Pacific Northwest.
#'
#' A dataset containing GPS coordinates, elevation and UTC Offset information
#' for 10 sites in the Pacific Northwest.
#'
#' @format A data frame with 10 rows and 5 variables:
#' \describe{
#'      \item{Site}{Location abbreviation}
#'      \item{Latitude}{Latitude coordinate}
#'      \item{Longitude}{Longitude coordinate}
#'      \item{Elevation}{Elevation at the location, in meters}
#'      \item{TZ}{UTC Offset}
#' }
#'
"locations"

#' Sample solar irradiance dataset.
#'
#' A dataset of measured global horizontal irradiance (GHI) from Eugene, Oregon
#' for the month of September, 2014.
#'
#' @format A data.frame with 7 columns:
#' \describe{
#'      \item{Year}{2014}
#'      \item{Month}{September}
#'      \item{DayOfMonth}{The day of the month, 1 through 30}
#'      \item{DayOfYear}{The day of the year, 244 through 273}
#'      \item{Interval}{Time between observations in minutes}
#'      \item{Minutes}{Minutes elapsed since midnight}
#'      \item{GHI}{Measured global horizontal solar irradiance}
#' }
#'
#' @source
#' Raw data available through the University of Oregon Solar Radiation
#' Monitoring Lab website:
#'
#' http://solardat.uoregon.edu/SelectArchival.html
#'
"eugene"

## For Rcpp
#' @useDynLib clearskies

#' @importFrom Rcpp sourceCpp

.onUnload <- function(libpath) {
    # Reference: r-pkgs.had.co.nz.html#src
    library.dynam.unload('clearskies', libpath)
}

