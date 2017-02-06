## Clear sky models to be implemented

BD <- function(dayofyear, year, tz, latitude, longitude, interval, ...,
               parameters = c(a = 0.70)) {

    a = parameters[['a']]
    io = exrad(dayofyear, times = 60L*24L/interval)
    z = zenith(dayofyear, year, tz, latitude, longitude, interval)
    ghi = a * io * cos(z*pi/180)
    return(ghi)
}

DPP <- function(dayofyear, year, tz, latitude, longitude, interval, ...,
                parameters = c(a = 950.2, b = -0.075, c = 14.29, d = 21.04)) {

    a = parameters[['a']]; b = parameters[['b']]; c = parameters[['c']]
    d = parameters[['d']]

    z = zenith(dayofyear, year, tz, latitude, longitude, interval)
    dni = a * (1 - exp(c * (90 - z)))
    diffuse = c + d * (pi/2 - z*pi/180)
    ghi = dni * cos(z*pi/180) + diffuse
    return(ghi)
}

Haurwitz <- function(dayofyear, year, tz, latitude, longitude, interval, ...,
                     parameters = c(a = 1098,b = -0.057)) {

    a = parameters[['a']]; b = parameters[['b']]

    z = zenith(dayofyear, year, tz, latitude, longitude, interval)
    ghi = a * cos(z*pi/180)* exp(b / cos(z*pi/180))
    return(ghi)
}

KC <- function(dayofyear, year, tz, latitude, longitude, interval, ...,
               parameters = c(a = 910, b = 30)) {

    a = parameters[['a']]; b = parameters[['b']]
    z = zenith(dayofyear, year, tz, latitude, longitude, interval)
    ghi = a * cos(z*pi/180) - b
    return(ghi)
}

Laue <- function(dayofyear, year, tz, latitude, longitude, interval, elevation, ...) {

    # elevation may be null if using clear_sky
    if (is.null(elevation) || missing(elevation))
        stop('Elevation is required')

    z = zenith(dayofyear, year, tz, latitude, longitude, interval)
    io = exrad(dayofyear, times = 60L*24L/interval)
    am = 1/cos(z*pi/180)
    dni = io * ((1-0.14*elevation) * 0.7^(am^0.678) + 0.14 * elevation)
    diffuse = 14.29 + 21.04 * (pi/2 - z*pi/180)
    ghi = dni * cos(z*pi/180) + diffuse
    return(ghi)
}

# TODO: use Ineichen as model for determing which constants are parameters
# same for Meinel
Laue <- function(dayofyear, year, tz, latitude, longitude, interval, elevation, ...,
                 parameters = c(a =, b =)) {

    # elevation may be null if using clear_sky
    if (is.null(elevation) || missing(elevation))
        stop('Elevation is required')

    a = parameters[['a']]; b = parameters[['b']]; c = parameters[['c']]
    d = parameters[['d']]

    z = zenith(dayofyear, year, tz, latitude, longitude, interval)
    io = exrad(dayofyear, times = 60L*24L/interval)
    AM = 1 / cos(z*pi/180)
    dni = io * ((1-0.14*elevation) * 0.7^(AM^0.678) + 0.14 * elevation)
    diffuse = 14.29 + 21.04 * (pi/2 - z*pi/180)
    ghi = dni * cos(z*pi/180) + diffuse
    return(ghi)
}


Meinel <- function(dayofyear, year, tz, latitude, longitude, interval, ...) {

    io = exrad(dayofyear, times = 60L*24L/interval)
    z = zenith(dayofyear, year, tz, latitude, longitude, interval)
    AM = 1/cos(z*pi/180)
    dni = io * 0.7^(AM^0.678)
    diffuse = 14.29 + 21.04 * (pi/2 - z*pi/180)
    ghi = dni * cos(z*pi/180) + diffuse
    return(ghi)
}

# alt
clear_sky <- function(model, x, y, data,
                      dayofyear, year, interval,
                      tz, latitude, longitude,
                      elevation, parameters) {

    has_data = !missing(data)
    has_parameters = !missing(parameters)

    if (has_data && !is.numeric(data))
        stop('data must be numeric')

    if (!missing(x) && !length(names(x)))
        stop('x must be named')

    if (!missing(y) && !length(names(y)))
        stop('y must be named')

    if (has_parameters && !length(names(parameters)))
        stop('parameters must be named')

    if (is.character(model))
        model.name = model
    else if (is.function(model))
        model.name = as.character(substitute(model))
    else
        stop('invalid model')

    # Unpack arguments if using x and/or y
    if (!missing(x)) {
        x = as.list(x)
        names(x) = tolower(names(x))
        dayofyear = x$dayofyear
        year = x$year
        interval = x$interval
    }

    if (!missing(y)) {
        y = as.list(y)

        if (any( lapply(y, length) > 1))
            stop('Too many values in y')

        names(y) = tolower(names(y))
        names(y)[names(y) == 'tz'] = 'timezone'

        tz = y$timez
        longitude = y$long
        latitude = y$lat
        # elevation isn't used in all models, check if it was passed
        elevation = if (length(y$elev)) y$elev else NULL
    }

    interval = unique(interval)
    if (length(interval) > 1 || missing(interval))
        stop('Interval must contain a single unique value')

    #model = match.fun(model)
    model = get(model, envir = asNamespace('clearskies'))

    if (has_parameters)
        fit = model(dayofyear = dayofyear, year = year, tz = tz,
                    latitude = latitude, longitude = longitude,
                    interval = interval, elevation = elevation,
                    parameters = parameters)
    else
        fit = model(dayofyear = dayofyear, year = year, tz = tz,
                    latitude = latitude, longitude = longitude,
                    interval = interval, elevation = elevation)

    object = list(model = model.name,
                  observed = if (has_data) data else NULL,
                  predicted = fit,
                  time.interval = interval)
    structure(object, class = 'clearsky')
}

