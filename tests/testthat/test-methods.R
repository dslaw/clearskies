context('Evaluation: S3 methods for clearsky class')

clear_pts <- function(x, cs, thresholds, window_len = 10L) {
    ## Stub
    n <- length(x)

    if (n != length(cs))
        stop('x must be the same length as cs')
    if (window_len <= 0 || window_len > n)
        stop('Incorrect value to window_len')
    if (length(thresholds) != 5)
        stop('Thresholds must be a list of length 5')

    return(!logical(n))
}

test_that('clear_points method dispatches correctly for clearsky', {
    obj = list(observed = 1:10, predicted = 1:10, model = 'RS')
    class(obj) = 'clearsky'
    result = clear_points(obj, thresholds, 10L)
    expect_identical(result$clear, !logical(10))
})

