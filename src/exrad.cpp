#include <Rcpp.h>

//' Helper function for calculating solar irradiance in clear sky models.
//'
//' @param dayofyear Day of year to calculate solar irradiance for. May be a
//' vector.
//' @param times Length of return vector. Passed to \emph{rep}.
//'
//' @return A vector of earth radius values of length times.
//'
//' @keywords internal
// [[Rcpp::export]]
Rcpp::NumericVector exrad(Rcpp::NumericVector dayofyear, int times) {
    const double deg2rad = M_PI / 180;

    Rcpp::NumericVector dayangle = 360.0 * (dayofyear - 1.0) / 365.0;
    Rcpp::NumericVector d2 = 2.0 * dayangle;

    // Earth radius vector * solar constant = solar energy
    Rcpp::NumericVector erv = 1.00011 + (0.034221 * Rcpp::cos(deg2rad * dayangle)) + (0.00128 * Rcpp::sin(deg2rad * dayangle));
    erv += 0.000719 * (Rcpp::cos(deg2rad * d2)) + (0.000077 * Rcpp::sin(deg2rad * d2));
    erv = 1366.1 * erv;

    Rcpp::NumericVector ret = rep_each(erv, times);
    return ret;
}

