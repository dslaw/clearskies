#include <algorithm>   // replace
#include <stdexcept>   // range_error
#include <Rcpp.h>

#include <map>

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
double L(Rcpp::NumericVector x) {
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
double sigma(Rcpp::NumericVector x) {

    double sigma;
    Rcpp::NumericVector s = diff(x);

    sigma = sd(s) / mean(x);

    if (std::isnan(sigma) || std::isinf(sigma))
        // catch division by 0
        return 0.0;

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
double S(Rcpp::NumericVector x, Rcpp::NumericVector cs) {

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
std::map<int, double> calculate_criterion(Rcpp::NumericVector x, Rcpp::NumericVector cs) {

    std::map<int, double> result;

    double mean_diff = mean(x) - mean(cs);
    double max_diff = max(x) - max(cs);
    double line_len_diff = L(x) - L(cs);
    double sigma_diff = sigma(x) - sigma(cs);
    double max_dev = S(x, cs);

    result[0] = mean_diff;
    result[1] = max_diff;
    result[2] = line_len_diff;
    result[3] = sigma_diff;
    result[4] = max_dev;

    return result;
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
bool evaluate_criterion(std::map<int, double> criterion, Rcpp::List thresholds) {
    // Compare clear skies criterion to threshold values.
    // All criterion must be between their respective thresholds (inclusive)
    // to return True (clear).
    // thresholds must be checked and ordered in R
    // i.e. must be length 5

    std::map<int, double>::iterator i;
    Rcpp::List::iterator j = thresholds.begin();
    Rcpp::NumericVector bounds;
    double criteria;

    for (i = criterion.begin(); i != criterion.end(); ++i, ++j) {
        // comparison may not work correctly if dereferenced iterators are
        // compared directly
        bounds = *j;
        criteria = i->second;

        if (criteria < min(bounds) || criteria > max(bounds)) {
            return false;
        }
    }

    return true;
}

//' Clear sky detection.
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
//' @export
// [[Rcpp::export]]
Rcpp::LogicalVector clear_points(Rcpp::NumericVector x, Rcpp::NumericVector cs,
                                 Rcpp::List thresholds, int window_len) {

    int n = x.size();

    if (n != cs.size())
        throw std::range_error("x must be the same length as cs");
    if (window_len <= 0 || window_len > n)
        throw std::range_error("Incorrect value to window_len");
    if (thresholds.size() != 5)
        throw std::range_error("Thresholds must be a list of length 5");

    bool allclear;
    std::map<int, double> criterion;

    Rcpp::LogicalVector clear(n);
    Rcpp::NumericVector obs(window_len);
    Rcpp::NumericVector pred(window_len);

    Rcpp::NumericVector::iterator i;
    Rcpp::NumericVector::iterator j = cs.begin();
    Rcpp::LogicalVector::iterator k = clear.begin();

    for (i = x.begin(); i != x.end() - window_len + 1; ++i, ++j, ++k) {
        obs.assign(i, i + window_len);
        pred.assign(j, j + window_len);

        criterion = calculate_criterion(obs, pred);
        allclear = evaluate_criterion(criterion, thresholds);

        if (allclear) {
            std::replace(k, k + window_len, false, true);
        }

        Rcpp::checkUserInterrupt();
    }

    return clear;
}

//' Root mean squared error.
//'
//' @param x Numeric vector.
//' @param y Numeric vector.
//'
//' @return Square root of the mean squared error between x and y.
//'
//' @export
// [[Rcpp::export]]
double rmse(Rcpp::NumericVector x, Rcpp::NumericVector y) {
    // Root mean squared error

    double mse = mean(pow(x - y, 2));
    return sqrt(mse);
}

