context('Check clear sky detection')

load('clearpts.RData') # clear5, clear10 for Sept 2014 @ Eugene

# Test that guards return errors
# 1. length of vectors x and cs differ
# 2. window len is incorrect size - <= 0 or > length of x
# 3. length of thresholds not equal to 5
test_that('clear_points raises errors', {
    x = rnorm(20)
    y = rnorm(20)

    expect_error(clear_points(x, rnorm(10), thresholds, window_len = 10L),
                 'x must be the same length as cs')
    expect_error(clear_points(rnorm(35), y, thresholds, window_len = 10L),
                 'x must be the same length as cs')
    expect_error(clear_points(x, y, thresholds, window_len = 0),
                 'Incorrect value to window_len')
    expect_error(clear_points(x, y, thresholds, window_len = -10),
                 'Incorrect value to window_len')
    expect_error(clear_points(x, y, thresholds, window_len = 25),
                 'Incorrect value to window_len')

    toomanythresholds = list(c(-1, 1), c(-2, 1), c(0, 1), c(-0.5, 1), c(-10, 10), c(1, 3)) # 6
    toofewthresholds = list(c(-1, 1), c(-2, 1), c(0, 1), c(-0.5, 1)) # 4

    expect_error(clear_points(x, y, toomanythresholds, window_len = 10),
                 'Thresholds must be a list of length 5')
    expect_error(clear_points(x, y, toofewthresholds, window_len = 10),
                 'Thresholds must be a list of length 5')
})


# Test that results are correct
model = clear_sky('RS', x = unique(eugene[, c('Year', 'DayOfYear', 'Interval')]),
                   locations[3, ])
fit = model$predicted
ghi = eugene[['Ghi']]
testclear10 = clear_points(x = ghi, cs = fit, thresholds, window_len = 10)
testclear5 = clear_points(x = ghi, cs = fit, thresholds, window_len = 5)

test_that('clear_points returns correct results', {
    len = 50
    expect_equal(clear_points(1:len, 1:len, thresholds, window_len = 1),
                 !logical(len))
    expect_equal(clear_points(1:len, 1:len, thresholds, window_len = 10),
                 !logical(len))

    expect_identical(testclear5, clear5)
    expect_identical(testclear10, clear10)
})

