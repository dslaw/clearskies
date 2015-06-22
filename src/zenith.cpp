#include <algorithm>   // transform, copy
#include <stdexcept>   // range_error
#include <math.h>      // floor(double), sin(double), cos(double), atan2
#include <numeric>     // iota
#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]
// c++11 required for lambdas and iota


double calc_julian_day(double year, double dayofyear) {
    return 32916.5 + (year - 1949)*365 + (floor((year - 1949)/4)) + dayofyear;
}

Rcpp::NumericVector julian_day(Rcpp::NumericVector dayofyear, Rcpp::NumericVector year) {

    int n_days = dayofyear.size();
    int n_years = year.size();

    // recycle shorter vector to length of longer vector
    if (n_years > n_days) {
        dayofyear = rep_len(dayofyear, n_years);
    } else if (n_days > n_years) {
        year = rep_len(year, n_days);
    }

    Rcpp::NumericVector ret(year.size());

    std::transform(year.begin(), year.end(), dayofyear.begin(),
            ret.begin(), calc_julian_day);
    return ret;
}

Rcpp::NumericVector universal_gmt(int interval, double tz) {

    if (interval > 60 || interval < 1)
        throw std::range_error("Interval must be between 1 and 60");

    int n = 1440 / interval; // 60 * 24 = 1440 minutes/day
    int step = 60 / interval;

    // using iota gives a speed increase
    // since this function returns a fixed length no matter what,
    // the speed increase isn't really worth it
    Rcpp::IntegerVector unique_hours = Rcpp::seq_len(24);
    unique_hours = unique_hours - 1;
    Rcpp::IntegerVector::iterator it = unique_hours.begin();

    // faster than using rep_len
    Rcpp::NumericVector hour(n);
    Rcpp::NumericVector::iterator i;

    for (i = hour.begin(); i != hour.end(); i += step, ++it) {
        std::fill(i, i + step, *it);
    }

    Rcpp::IntegerVector unique_minutes = Rcpp::seq_len(60);
    unique_minutes = unique_minutes - 1;
    Rcpp::NumericVector minute;
    minute = rep(unique_minutes, n);

    // overwrite hour instead of creating a new return vector
    std::transform(hour.begin(), hour.end(), minute.begin(), hour.begin(),
        [tz](double h, double m) { return (h*3600 + m*60)/3600 - tz; }
    );

    return hour;
}

void setnum(Rcpp::NumericVector & x, double c) {
    std::transform(x.begin(), x.end(), x.begin(),
            [c](double a) {
                a = a - c * floor(a/c);
                if (a < 0) {
                    return a + c;
                } else {
                    return a;
                }
            });
}

//' Calculate the zenith angle.
//'
//' @param dayofyear Numeric vector containing the day of year(s),
//' for which the zenith angle should be calculated.
//' @param year Numeric vector containing the year(s) for which the zenith
//' angle sould be calculated.
//' @param tz UTC Offset. Ex: Eastern Standard Time = -5.
//' @param latitude Latitude of the location at which the zenith angle is to
//' be calculated.
//' @param longitude Longitude of the location at which the zenith angle is to
//' be calculated.
//' @param interval Number of minutes between zenith angle calculations. Defaults to
//' (every minute). Must be an integer between 1 and 60, inclusive.
//'
//' @return A single vector of the zenith angles at each interval throughout the
//' specified time period
//' (i.e. a vector of length (60 * 24 / interval) * number of days).
//'
//' @details
//' zenith is vectorized over both dayofyear and year, with the shorter vector
//' being recycled as usual.
//'
//' @keywords internal
// [[Rcpp::export]]
Rcpp::NumericVector zenith(Rcpp::NumericVector dayofyear, Rcpp::NumericVector year,
                           double tz, double latitude, double longitude,
                           int interval = 1) {

    double const deg2rad = M_PI/180;
    double const rad2deg = 180/M_PI;

    Rcpp::NumericVector universaltime = universal_gmt(interval, tz);
    Rcpp::NumericVector julday = julian_day(dayofyear, year);

    // to vectorize over dayofyear and year,
    // create a copy of universaltime for each julday
    int n = universaltime.size();
    Rcpp::NumericVector ecliptic_time = Rcpp::rep_each(julday, n);
    Rcpp::NumericVector utime = Rcpp::rep_len(universaltime, ecliptic_time.size());

    // Time used in the calculation of ecliptic coordinates
    ecliptic_time = ecliptic_time + utime / 24;
    ecliptic_time = ecliptic_time - 51545;

    // Mean longitude, all values between 0 and 360
    Rcpp::NumericVector meanlong = 280.46 + 0.9856474 * ecliptic_time;
    setnum(meanlong, 360.0);

    // Mean anomaly
    Rcpp::NumericVector meananom = 357.528 + 0.9856003 * ecliptic_time;
    setnum(meananom, 360.0);
    meananom = meananom * deg2rad; // radians

    // Ecliptic longitude
    Rcpp::NumericVector eclipticlong = meanlong + 1.915 * Rcpp::sin(meananom) + 0.02 * Rcpp::sin(2*meananom);
    setnum(eclipticlong, 360.0);
    eclipticlong = eclipticlong * deg2rad; // radians

    // Obliquity of the ecliptic
    Rcpp::NumericVector eclipticobli = 23.439 - 0.0000004 * ecliptic_time;
    eclipticobli = eclipticobli * deg2rad; // radians

    // Declination
    // declin is only ever used in the context of deg2rad * declin
    // but removing both rad2deg and deg2rad later causes (slight) numerical differences from R
    // when calculating ETR solar zenith angle
    // no idea why...
    // but leave in (even though they should cancel out)
    Rcpp::NumericVector declin = rad2deg * Rcpp::asin( Rcpp::sin(eclipticobli) * Rcpp::sin(eclipticlong) );

    // Right ascension
    // single pass, avoid creating vectors for top and bottom
    Rcpp::NumericVector rascen(eclipticlong.size());
    std::transform(eclipticlong.begin(), eclipticlong.end(), eclipticobli.begin(), rascen.begin(),
            [rad2deg](double eclong, double ecobli) {
                double top = cos(ecobli) * sin(eclong);
                double bottom = cos(eclong);
                double rightascen = rad2deg * atan2(top, bottom);

                // ensure angle is positive
                if (rightascen < 0) {
                    return rightascen + 360.0;
                } else {
                    return rightascen;
                }
            });

    // Greenwich mean sidereal time
    Rcpp::NumericVector gmst = 6.697375 + 0.0657098242 * ecliptic_time + utime;
    setnum(gmst, 24.0);

    // Local mean sidereal time
    Rcpp::NumericVector lmst = gmst * 15 + longitude;
    setnum(lmst, 360.0);

    // Hour angle, between -180 and 180 degrees
    Rcpp::NumericVector hour_angle = lmst - rascen;
    std::transform(hour_angle.begin(), hour_angle.end(), hour_angle.begin(),
            [](double a) {
                if (a < -180) {
                    return a + 360;
                } else if (a > 180) {
                    return a - 360;
                } else {
                    return a;
                }
            });

    // ETR solar zenith angle
    Rcpp::NumericVector sd = Rcpp::sin(deg2rad * declin);
    Rcpp::NumericVector cd = Rcpp::cos(deg2rad * declin);
    Rcpp::NumericVector ch = Rcpp::cos(deg2rad * hour_angle);
    Rcpp::NumericVector cz = sd * sin(deg2rad * latitude) + cd * cos(deg2rad * latitude) * ch;

    // cos of zenith must be between -1 and 1
    std::replace_if(cz.begin(), cz.end(),
            [](double a) { return a < -1; },
            -1);
    std::replace_if(cz.begin(), cz.end(),
            [](double a) { return a > 1; },
            1);

    Rcpp::NumericVector zenetr = Rcpp::acos(cz) * rad2deg;

    // limit the degrees below the horizon to 90
    std::replace_if(zenetr.begin(), zenetr.end(),
            [](double a) { return a > 90; },
            90);

    return zenetr;
}

