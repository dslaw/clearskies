#' Clear sky detection
#'
#' Implementation of the clear sky detection algorithm found in Reno et al,
#' 2012. (See References). Determines clear points using a rolling window and
#' five criterion.
#'
#' A point is declared clear if it is determined to be clear at least once.
#'
#' @param x Numeric vector of measured irradiance values or object of clear_sky
#' class containing both observed and predicted members.
#' @param cs Numeric vector of predicted irradiance from a clear sky model.
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
#' @param window_len Length of window to use in calculating criterion. Must be
#' a positive integer.
#' @param ... ignored.
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

#' @rdname clear_points
#' @export
clear_points.default <- function(x, cs, thresholds, window_len, ...) {
    clear_pts(x, cs, thresholds, window_len)
}

#' @rdname clear_points
#' @export
clear_points.clearsky <- function(x, thresholds, window_len, ...) {

    stopifnot( inherits(x, 'clearsky') )

    clear <- clear_pts(x = x$observed, cs = x$predicted,
                          thresholds = thresholds, window_len = window_len)
    x$clear <- clear
    x
}

#' @export
summary.clearsky <- function(object, ...) {

    stopifnot( inherits(object, 'clearsky') )

    cat('Model:', object$model)
    cat('\n')

    day_length = 1440 / object$time.interval
    n = length(object$predicted)
    number_days = floor(n / day_length)
    cat(n, 'predicted points over', number_days, 'days')
    cat('\n\n')

    if (length(object$observed)) {
        cat('Observed:\n')
        print(summary(object$observed, ...))
        cat('\n')
    }

    cat('Predicted:\n')
    print(summary(object$predicted, ...))
    cat('\n')

    if (length(object$clear)) {
        cat('Number of clear points:', sum(object$clear), ' ')
        avg = round(mean(object$clear), 4L)
        cat('Percent clear:', paste0(avg * 100, '%'))
        cat('\n')
    }

    invisible()
}

#' @export
plot.clearsky <- function(x, ..., use.ggplot = FALSE) {

    stopifnot( inherits(x, 'clearsky') )

    if (is.null(pred <- x$predicted))
        stop('No predicted model to plot')

    obs = x$observed
    clear = x$clear
    n = max(length(pred), length(obs))
    ix = seq_len(n)

    if (is.null(obs)) {
        obs = rep(NA, n)
        clear = NULL
    }

    # bring clear points to the forefront
    clear_alpha = 0.8
    alpha = clear_alpha * 0.35

    line_width = 1.5

    if (requireNamespace('ggplot2', quietly = TRUE) && use.ggplot) {
        y = data.frame(Type = rep(c('Observed', 'Predicted'), each = length(ix)),
                       Index = rep(ix, 2),
                       GHI = c(obs, pred))

        p = ggplot2::ggplot(y, ggplot2::aes_string(x = 'Index', y = 'GHI',
                                                   color = 'Type'))

        if (!is.null(clear)) {
            clear = rep_len(clear, nrow(y))
            y = cbind(y, Alpha = ifelse(clear, clear_alpha, alpha))
            p = ggplot2::ggplot(y, ggplot2::aes_string(x = 'Index', y = 'GHI',
                                                   color = 'Type',
                                                   alpha = 'Alpha'))
            p = p + ggplot2::geom_line(ggplot2::aes_string(alpha = 'Alpha'))
            p = p + ggplot2::scale_alpha_identity()
        } else {
            p = ggplot2::ggplot(y, ggplot2::aes_string(x = 'Index', y = 'GHI',
                                                   color = 'Type'))
            p = p + ggplot2::geom_line(alpha = alpha)
        }

        p = p + ggplot2::guides(color = ggplot2::guide_legend(title = ''))
        p + ggplot2::labs(x = '', y = 'GHI')
    } else {
        y_range = c(min(pred, obs, na.rm = TRUE), max(pred, obs, na.rm = TRUE))

        obs_color = adjustcolor('red', alpha.f = alpha)
        pred_color = adjustcolor('cyan', alpha.f = alpha)
        obs_clear_color = adjustcolor('red', alpha.f = clear_alpha)
        pred_clear_color = adjustcolor('cyan', alpha.f = clear_alpha)

        if (!is.null(clear)) {
            pred_col = ifelse(clear, pred_clear_color, pred_color)
            obs_col = ifelse(clear, obs_clear_color, obs_color)

            plot(ix, pred, type = 'n', ylim = y_range, ylab = 'GHI', xlab = '')
            segments(head(ix, -1), head(pred, -1), ix[-1], pred[-1], pred_col,
                     lwd = line_width)
            segments(head(ix, -1), head(obs, -1), ix[-1], obs[-1], obs_col,
                     lwd = line_width)
        } else {
            plot(ix, pred, type = 'l', ylim = y_range, ylab = 'GHI', xlab = '',
                 col = pred_color, lwd = line_width)
            lines(ix, obs, col = obs_color, lwd = line_width)
        }
    }
}

