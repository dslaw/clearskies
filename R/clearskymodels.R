## Clear sky models
##
## Models can be fit by passing each argument or using the more generic
## clear_sky which takes two named dataframes, vectors or lists
## x and y, containing the relevant time information and location information,
## respectively.
##
## Ellipsis in model parameters allow extra arguments to be passed through
## the wrapper function and ignored if they're not used

#' Fit clear sky model.
#'
#' @param model Name of model to be fit.
#' @param x Dataframe, named list or named vector containing date information
#' for model. Must have elements named 'DayOfYear', 'Year' and 'Interval'.
#' Alternatively, dayofyear, year and interval can be passed directly to
#' clear_sky as arguments.
#' @param y Dataframe, named list or named vector containing location information
#' for model. Must have elements named 'Latitude', 'Longitude', 'Timezone' or
#' 'TZ' and, if necessary for the model, 'Elevation'. Alternatively,
#' latitude, longitude, timezone and elevation can be passed directly to
#' clear_sky as arguments.
#' @param data Vector of observed data corresponding to the model. Optional.
#' @param dayofyear A vector of the day(s) of the year that the model is to be
#' fit to. Ignore if using x.
#' @param year A vector of the year(s) that the model is to be fit to. If using
#' x, ignore.
#' @param interval Interval, in minutes, for which the model is to be fit,
#' starting from midnight. Ignore if using x.
#' @param tz Timezone/UTC Offset. Ignore if using y.
#' @param latitude Latitude of the location for the model. Ignore if using y.
#' @param longitude Longitude of the location for the model. Ignore if using y.
#' @param elevation Elevation of the location for the model. Ignore if using y.
#' @param parameters Optional named vector or list of parameters for the model.
#'
#' @return An object of class 'clearsky' containing named elements observed,
#' predicted and model; predicted contains the modeled GHI values for each day
#' for the specified interval.
#'
#' @examples
#' # Fit Robledo-Soler model to the year of 2014 for Eugene, Oregon
#'
#' time_info <- list(DayOfYear = 1:365,
#'                   Year = 2014,
#'                   Interval = 1)
#' location_info <- list(Latitude = 44.05,
#'                       Longitude = -123.07,
#'                       TZ = -8)
#' model_params <- c(a = 1000, b = 1.5, c = 0.2)
#'
#' model <- clear_sky('RS', x = time_info, y = location_info,
#'                    parameters = model_params)
#'
#' # Alternatively, pass time and location arguments direct to the model
#' model <- clear_sky('RS', dayofyear = 1:365, year = 2014, interval = 1,
#'                    latitude = 44.05, longitude = -123.07, tz = -8)
#'
#' @export
clear_sky <- function(model, x, y, data,
                      dayofyear, year, interval,
                      tz, latitude, longitude,
                      elevation, parameters) {

    has_data = !missing(data)

    if (has_data && !is.numeric(data))
        stop('data must be numeric')

    if (!missing(interval) && length(interval) != 1)
        stop('Interval must contain a single unique value')

    if (missing(x))
        x = list(DayOfYear = dayofyear,
                 Year = year,
                 Interval = interval)

    if (missing(y))
        y = list(Latitude = latitude,
                 Longitude = longitude,
                 Elevation = if (!missing(elevation)) elevation else NULL,
                 TZ = tz)

    model.name = as.character(substitute(model))
    model = .pass_args(model)

    if (!missing(parameters))
        fit = model(x = x, y = y, parameters = parameters)
    else
        fit = model(x = x, y = y)


    object = list(observed = if (has_data) data else NULL,
                predicted = fit,
                model = model.name)
    structure(object, class = 'clearsky')
}

.pass_args <- function(model) {
    model = match.fun(model)

    inner <- function(x, y, ...) {

        if (!length(names(x)) || !length(names(y)))
            stop('x and y must be named')

        if (any( lapply(y, length) > 1 ))
            stop('Too many values in y')

        # convert to list to use '$' for fuzzy matching of names
        x = as.list(x)
        y = as.list(y)

        names(x) = tolower(names(x))
        names(y) = tolower(names(y))
        names(y)[names(y) == 'tz'] = 'timezone'

        interval = unique(x$interval)
        if (!length(interval)) stop('Missing interval')
        if (length(interval) > 1) stop('Interval must contain a single unique value')

        year = x$year
        dayofyear = x$dayofyear

        tz = y$timez
        long = y$long
        lat = y$lat

        # elevation isn't used in all models, check if it was passed
        elev = if (length(y$elev)) y$elev else NULL

        return(model(dayofyear = dayofyear, year = year, tz = tz,
                     latitude = lat, longitude = long, interval = interval,
                     elevation = elev, ...))
    }

    return(inner)
}

#' Fit Adnot-Bourges-Campana-Gicquel (ABCG) clear sky model.
#'
#' @param dayofyear The day of year to fit the model to. May be either a single
#' value or a vector.
#' @param year The year to fit the model to. The model is fit over both
#' dayofyear and year, with the shorter vector being recycled as normal.
#' @param tz UTC Offset. Ex: Eastern Standard Time = -5.
#' @param latitude Latitude at the location for which the model is to be fit.
#' @param longitude Longitude at the location for which the model is to be fit.
#' @param interval Number of minutes between clear sky points. Defaults to 1
#' (every minute). Must be an integer between 1 and 60, inclusive.
#' @param parameters Adnot-Bourges-Campana-Gicquel model parameters. Named
#' vector or list containing values for a, b and c.
#'
#' @return Vector of fitted irradiance values for the given time period.
#'
#' @name ABCG
#'
#' @keywords internal
ABCG <- function(dayofyear, year, tz, latitude, longitude, interval, ...,
                 parameters = c(a = 951.39, b = 1.15)) {

    a = parameters[['a']]; b = parameters[['b']]
    z = zenith(dayofyear, year, tz, latitude, longitude, interval)
    ghi = a * cos(z*pi/180)^b
    return(ghi)
}

#' Fit Robledo-Soler (RS) clear sky model.
#'
#' @param dayofyear The day of year to fit the model to. May be either a single
#' value or a vector.
#' @param year The year to fit the model to. The model is fit over both
#' dayofyear and year, with the shorter vector being recycled as normal.
#' @param tz UTC Offset. Ex: Eastern Standard Time = -5.
#' @param latitude Latitude at the location for which the model is to be fit.
#' @param longitude Longitude at the location for which the model is to be fit.
#' @param interval Number of minutes between clear sky points. Defaults to 1
#' (every minute). Must be an integer between 1 and 60, inclusive.
#' @param parameters Robledo-Soler model parameters. Named vector or list
#' containing values for a, b and c.
#'
#' @return Vector of fitted irradiance values for the given time period.
#'
#' @name RS
#'
#' @keywords internal
RS <- function(dayofyear, year, tz, latitude, longitude, interval, ...,
               parameters = c('a' = 1159.24, 'b' = 1.179, 'c' = -0.0019)) {

    a = parameters[['a']]; b = parameters[['b']]; c = parameters[['c']]

    z = zenith(dayofyear, year, tz, latitude, longitude, interval)
    ghi = a * cos(z*pi/180)^b * exp(c*(90-z))
    return(ghi)
}

#' Fit Ineichen-Perez clear sky model.
#'
#' @param dayofyear The day of year to fit the model to. May be either a single
#' value or a vector.
#' @param year The year to fit the model to. The model is fit over both
#' dayofyear and year, with the shorter vector being recycled as normal.
#' @param tz UTC Offset. Ex: Eastern Standard Time = -5.
#' @param latitude Latitude at the location for which the model is to be fit.
#' @param longitude Longitude at the location for which the model is to be fit.
#' @param interval Number of minutes between clear sky points. Defaults to 1
#' (every minute). Must be an integer between 1 and 60, inclusive.
#' @param parameters Ineichen-Perez model parameters. Named vector or list
#' containing values for a, b, c and TL (linke turbidity).
#'
#' @return Vector of fitted irradiance values for the given time period.
#'
#' @name Ineichen
#'
#' @keywords internal
Ineichen <- function(dayofyear, year, tz, latitude, longitude, interval, elevation,
                     parameters = c('a' = 0.50572, 'b' = 6.07995, 'c' = 1.6364, 'TL' = 3)) {

    # elevation may be null if using .pass_args
    if (is.null(elevation) || missing(elevation))
        stop('Elevation is required')

    a = parameters[['a']]; b = parameters[['b']]; c = parameters[['c']]
    TL = parameters[['TL']]

    # Linke turbidity values
    La = seq(90,-90, length = 2160)
    Lo = seq(-180, 180, length = 4320)

    I1 = which.min(abs(latitude-La))
    I2 = which.min(abs(longitude-Lo))

    io = exrad(dayofyear, times = 60L*24L/interval)
    z = zenith(dayofyear, year, tz, latitude, longitude, interval)

    AM = (cos(z*pi/180) + a * (90 + b - z) ** -c ) ** -1

    fh1 = exp(-elevation/8000)
    fh2 = exp(-elevation/1250)

    cg1 = 0.0000509 * elevation + 0.868
    cg2 = 0.0000392 * elevation + 0.0387

    ghi = cg1 * io * cos(z*pi/180)* exp(-cg2*AM*(fh1+fh2*(TL-1))) * exp(0.01*(AM)^(1.8))

    return(ghi)
}

