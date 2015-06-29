#' Clear sky detection.
#'
#' Determine clear points using a rolling window and five clear sky criterion.
#'
#' A point is declared clear if it is determined to be clear at least once.
#'
#' @param x Numeric vector of measured irradiance values.
#' @param cs Numeric vector of predicted irradiance from a clear sky model.
#' @param object Object of class clearsky containing observed and predicted
#' members. Used by corresponding method.
#' @param thresholds List of vectors, each vector containing the threshold
#' values for their respective clear sky criteria. Each vector must have length
#' greater than or equal to two, with the minimum and maximum values in the
#' vector being used as thresholds. The list must be arranged in the following
#' order:
#' \enumerate{
#'     \item Mean
#'     \item Max
#'     \item Line length
#'     \item Sigma
#'     \item Maximum deviation from clear sky slope
#' }
#' @param window_len Length of window, in minutes, used in calculating
#' criterion. Must be a positive integer.
#'
#' @return The form of the value returned by 'clear_points' depends on the class
#' of its argument.
#'
#' The default method returns a logical vector of the same length as x. TRUE
#' indicates that the corresponding measured irradiance value is clear.
#'
#' The clearsky method returns a clearsky object with member 'clear' set to a
#' logical vector of the same length as x.
#'
#' @section References:
#' Global Horizontal Irradiance Clear Sky Models: Implementation and Analysis,
#' Reno et al, 2012, pp. 28-36.
#'
#' @export
clear_points <- function(x, ...) UseMethod('clear_points')

#' @export
clear_points.default <- function(x, cs, thresholds, window_len) {
    clear_pts(x, cs, thresholds, window_len)
}

#' @export
clear_points.clearsky <- function(object, thresholds, window_len) {

    stopifnot( inherits(object, 'clearsky') )

    clear <- clear_pts(x = object$observed, cs = object$predicted,
                          thresholds = thresholds, window_len = window_len)
    object$clear <- clear
    object
}
