context('Evaluation: clear_sky')

# Test that inputs of different types are passed through
# 1. Named list
# 2. Dataframe
# 3. Named vector
# 4. individual arguments
# and that the output of each is:
# 1. the correct length
# 2. the correct type (numeric vector)
# 3. free of NaNs / NAs

model <- 'RS'
mdate <- data.frame(DayOfYear = 1:4, Year = rep(2012, 4), Interval = rep(1, 4))
mdate_lst <- list(DayOfYear = 1:4, Year = 2012, Interval = 1)
mdate_vec <- c(DayOfYear = 1, Year = 2012, Interval = 1)
site <- locations[3, ]
site_lst <- as.list(site)
site_vec <- as.vector(site)

test_that('dataframe arguments are passed', {
    result <- clear_sky(model, x = mdate,  y = site)
    result <- result$predicted
    expect_equal(length(result), 1440 * 4)
    expect_is(result, 'numeric')
    expect_equal(sum(is.na(result) | is.nan(result)), 0)
})

test_that('list arguments are passed', {
    result <- clear_sky(model, x = mdate_lst, y = site)
    result <- result$predicted
    expect_equal(length(result), 1440 * 4)
    expect_is(result, 'numeric')
    expect_equal(sum(is.na(result) | is.nan(result)), 0)

    result_y <- clear_sky(model, x = mdate, y = site_lst)
    result_y <- result_y$predicted
    expect_equal(length(result_y), 1440 * 4)
    expect_is(result_y, 'numeric')
    expect_equal(sum(is.na(result_y) | is.nan(result_y)), 0)
})

test_that('named vector arguments are passed', {
    result <- clear_sky(model, x = mdate_vec, y = site)
    result <- result$predicted
    expect_equal(length(result), 1440)
    expect_is(result, 'numeric')
    expect_equal(sum(is.na(result) | is.nan(result)), 0)

    result_y <- clear_sky(model, x = mdate, y = site_vec)
    result_y <- result_y$predicted
    expect_equal(length(result_y), 1440 * 4)
    expect_is(result_y, 'numeric')
    expect_equal(sum(is.na(result_y) | is.nan(result_y)), 0)
})

test_that('individual arguments are passed', {
    result <- clear_sky(model, dayofyear = 1:4, year = 2012, interval = 1,
                        latitude = 44.05, longitude = -123.07, tz = -8)
    result <- result$predicted
    expect_equal(length(result), 1440 * 4)
    expect_is(result, 'numeric')
    expect_equal(sum(is.na(result) | is.nan(result)), 0)
})

test_that('combinations of arguments are passed', {
    correct <- clear_sky(model, x = mdate, y = site)

    resultxargs <- clear_sky('RS', x = mdate, latitude = 44.05, longitude = -123.07, tz = -8)
    expect_identical(correct, resultxargs)

    resultyargs <- clear_sky('RS', y = site, dayofyear = 1:4, year = 2012, interval = 1)
    expect_identical(correct, resultyargs)
})

# Test that incorrect inputs produce errors:
# 1. More than 1 unique interval value
toomanyintervals <- list(DayOfYear = 1:4, Year = 2012, Interval = 1:4)
# 2. Unnamed object (list/vector) to x
mdate_noname <- list(1:4, 2012, 1)
# 3. Unnamed object (list/vector) to y
site_noname <- list(-144, 23, 150, -8)
# 4. Multiple values in a parameter of y
multiple_sites <- locations
# 5. Interval values < 1 or > 60 (guard in universal_gmt)
incorrect_interval0 <- list(DayOfYear = 1:4, Year = 2012, Interval = 0) # < 1
incorrect_interval70 <- list(DayOfYear = 1:4, Year = 2012, Interval = 70) # > 60
# 6. Missing elevation for Ineichen and Laue models (guard in Ineichen and Laue)
missing_elevation <- locations[8, c('Latitude', 'Longitude', 'TZ')]
# 7. Non-numeric data


test_that('.pass_args guards work ', {
    expect_error(clear_sky(model, x = mdate_noname, y = site),
                 'x and y must be named')
    expect_error(clear_sky(model, x = mdate, y = site_noname),
                 'x and y must be named')
    expect_error(clear_sky(model, x =  mdate, y = multiple_sites),
                 'Too many values in y')
})

test_that('interval guard works', {
    expect_error(clear_sky(model, x = toomanyintervals, y = site),
                 'Interval must contain a single unique value')
    expect_error(clear_sky(model, x = incorrect_interval0, y = site),
                 'Interval must be between 1 and 60')
    expect_error(clear_sky(model, x = incorrect_interval70, y = site),
                 'Interval must be between 1 and 60')
})

test_that('elevation guards work', {
    expect_error(clear_sky('Ineichen', x = mdate, y = missing_elevation),
                 'Elevation is required')
    expect_error(clear_sky('Laue', x = mdate, y = missing_elevation),
                 'Elevation is required')
})

test_that('Non-numeric guard works', {
    expect_error(clear_sky('RS', data = logical(10)),
                 'data must be numeric')
    expect_error(clear_sky('RS', data = letters),
                 'data must be numeric')
})

test_that('unnamed parameters raise an error', {
    model_params <- list(1000, 6, -0.09)
    expect_error(clear_sky(model, x = mdate, y = site, parameters = model_params))
})

test_that("elevation argument doesn't raise an error", {
    missing_elevation <- locations[8, c('Latitude', 'Longitude', 'TZ')]
    with_elevation <- locations[8, ]

    result_no_elev <- clear_sky('RS', mdate, missing_elevation)
    result_with_elev <- clear_sky('RS', mdate, with_elevation)
    expect_equivalent(result_no_elev, result_with_elev)
})

