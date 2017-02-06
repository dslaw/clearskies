#include <algorithm>   // replace
#include <stdexcept>   // range_error
#include <Rcpp.h>

//' Calculate line length variability.
//'
//' Line length variability is one of the five criterion used for detecting
//' clear points. It is defined as:
//' \deqn{
//' L = \sum_{i=1}^{n} \sqrt{ (GHI_{i+1} + GHI_{i})^2 + (t_{i+1} +
//' t_{i})^2 }
//' }
//' where \eqn{t} is the time series, and \eqn{GHI} is measured irradiance.
//'
//' @param x Numeric vector of measured irradiance.
//'
//' @section References:
//' Global Horizontal Irradiance Clear Sky Models: Implementation and Analysis,
//' Reno et al, 2012, pp. 30.
//'
//' @name L
//'
//' @keywords internal
//
double L(Rcpp::NumericVector &x) {
    // (t_(i+1) - t_(i))^2 is always 1
    return sum(sqrt(pow(diff(x), 2) + 1));
}

//' Normalized standard deviation of the slope between sequential points.
//'
//' The normalized standard deviation of the slope (sigma) is one of the five
//' criterion for detecting clear points. It is defined as follows:
//' \deqn{
//' \sigma = \frac{1}{ {\overline{GHI}} }
//' \sqrt{ \frac{1}{n-1} \sum_{i=1}^{n-1} (s_{i} {\overline{s}})^2 }
//' }
//' where \eqn{ {\overline{GHI}} } is the mean of the measured irradiance
//' values, \eqn{ s_{i} = GHI_{i+1} - GHI_{i} } and \eqn{ {\overline{s}} } is the
//' sample standard deviation of \eqn{s}.
//'
//' @param x Numeric vector of measured irradiance.
//'
//' @section References:
//' Global Horizontal Irradiance Clear Sky Models: Implementation and Analysis,
//' Reno et al, 2012, pp. 30.
//'
//' @name sigma
//'
//' @keywords internal
//
double sigma(Rcpp::NumericVector &x) {
    Rcpp::NumericVector s = diff(x);
    double sigma = sd(s) / mean(x);

    // catch division by 0
    if (std::isnan(sigma) || std::isinf(sigma)) {
        return 0.0;
    }

    return sigma;
}

//' Maximum deviation of the measured irradiance from the clear sky slope.
//'
//' The maximum deviation of the measured irradiance from the clear sky slope is
//' one of five criterion for detecting clear points. It is defined as follows:
//' \deqn{
//' S = max\{|s_{i}-d_{i}|\}
//' }
//' where \eqn{ s_{i} = GHI_{i+1} - GHI_{i} } and \eqn{ d_{i} = y_{i+1} - y_{i}
//' }. \eqn{GHI} denotes the measured irradiance and \eqn{y} denotes the
//' predicted irradiance from a clear sky model.
//'
//' @param x Numeric vector of measured irradiance.
//' @param cs Numeric vector of predicted irradiance. Should be the same
//' length as x, however this condition is not checked.
//'
//' @section References:
//' Global Horizontal Irradiance Clear Sky Models: Implementation and Analysis,
//' Reno et al, 2012, pp. 31.
//'
//' @name S
//'
//' @keywords internal
//
double S(Rcpp::NumericVector &x, Rcpp::NumericVector &cs) {
    return max(abs(diff(x) - diff(cs)));
}

//' Calculate five clear sky criterion.
//'
//' @param x Numeric vector of measured irradiance.
//' @param cs Numeric vector of predicted irradiance. Should be the same
//' length as x, however this condition is not checked.
//'
//' @name calculate_criterion
//'
//' @keywords internal
//
Rcpp::NumericVector calculate_criterion(Rcpp::NumericVector &x, Rcpp::NumericVector &cs) {
    Rcpp::NumericVector criterion(5);
    criterion[0] = mean(x) - mean(cs);    // mean difference
    criterion[1] = max(x) - max(cs);      // max difference
    criterion[2] = L(x) - L(cs);          // line length difference
    criterion[3] = sigma(x) - sigma(cs);  // sigma difference
    criterion[4] = S(x, cs);              // max deviance
    return criterion;
}

//' Check if all criterion are within their respective threshold values.
//'
//' @param criterion List of clear sky criterion.
//' @param thresholds List of vectors, each vector containing the threshold
//' values for their respective clear sky criteria.
//'
//' @return If all criterion are within their threshold ranges, inclusive,
//' returns true.
//'
//' Note: Clear sky criterion from \emph{criterion} are compared to values in
//' \emph{thresholds} by index, not name.
//'
//' @name evaluate_criterion
//'
//' @keywords internal
//
bool evaluate_criterion(Rcpp::NumericVector &criterion, Rcpp::List &thresholds) {
    // Compare clear skies criterion to threshold values.
    // All criterion must be between their respective thresholds (inclusive)
    // to return True (clear).
    // thresholds must be checked and ordered in R
    // i.e. must be length 5
    auto j = thresholds.begin();
    for (auto i = criterion.begin(); i != criterion.end(); ++i, ++j) {
        // comparison may not work correctly if dereferenced iterators are
        // compared directly
        Rcpp::NumericVector bounds = *j;
        double criteria = *i;

        if (criteria < min(bounds) || criteria > max(bounds)) {
            return false;
        }
    }

    return true;
}

//' Clear sky detection
//'
//' Determine clear points using a rolling window and five clear sky criterion.
//'
//' A point is declared clear if it is determined to be clear at least once.
//'
//' @param x Numeric vector of measured irradiance values.
//' @param cs Numeric vector of predicted irradiance from a clear sky model.
//' @param thresholds List of vectors, each vector containing the threshold
//' values for their respective clear sky criteria. Each vector must have length
//' greater than or equal to two, with the minimum and maximum values in the
//' vector being used as thresholds. The list must be arranged in the following
//' order:
//' \enumerate{
//'     \item Mean
//'     \item Max
//'     \item Line length
//'     \item Sigma
//'     \item Maximum deviation from clear sky slope
//' }
//' @param window_len Length of window, in minutes, used in calculating
//' criterion. Must be a positive integer.
//'
//' @return A logical vector of the same length as x, TRUE indicates the
//' point is clear.
//' @return A logical vector of the same length as x. TRUE indicates that the
//' corresponding measured irradiance value in x is clear.
//'
//' @section References:
//' Global Horizontal Irradiance Clear Sky Models: Implementation and Analysis,
//' Reno et al, 2012, pp. 28-36.
//'
// [[Rcpp::export]]
Rcpp::LogicalVector clear_pts(Rcpp::NumericVector x, Rcpp::NumericVector cs,
                              Rcpp::List thresholds, int window_len) {
    int n = x.size();

    if (n != cs.size())
        throw std::range_error("x must be the same length as cs");
    if (window_len <= 0 || window_len > n)
        throw std::range_error("Incorrect value to window_len");
    if (thresholds.size() != 5)
        throw std::range_error("Thresholds must be a list of length 5");

    Rcpp::LogicalVector clear(n);
    Rcpp::NumericVector obs(window_len);
    Rcpp::NumericVector pred(window_len);

    auto j = cs.begin();
    auto k = clear.begin();

    for (auto i = x.begin(); i != x.end() - window_len + 1; ++i, ++j, ++k) {
        obs.assign(i, i + window_len);
        pred.assign(j, j + window_len);

        auto criterion = calculate_criterion(obs, pred);
        bool allclear = evaluate_criterion(criterion, thresholds);

        if (allclear) {
            std::replace(k, k + window_len, false, true);
        }

        Rcpp::checkUserInterrupt();
    }

    return clear;
}

//' Root mean squared error
//'
//' Calculate root mean squared error.
//'
//' @param x Numeric vector.
//' @param y Numeric vector.
//'
//' @return Square root of the mean squared error between x and y.
//'
//' @export
// [[Rcpp::export]]
double rmse(Rcpp::NumericVector x, Rcpp::NumericVector y) {
    double mse = mean(pow(x - y, 2));
    return sqrt(mse);
}

