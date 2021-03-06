% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{L}
\alias{L}
\title{Calculate line length variability.}
\arguments{
\item{x}{Numeric vector of measured irradiance.}
}
\description{
Line length variability is one of the five criterion used for detecting
clear points. It is defined as:
\deqn{
L = \sum_{i=1}^{n} \sqrt{ (GHI_{i+1} + GHI_{i})^2 + (t_{i+1} +
t_{i})^2 }
}
where \eqn{t} is the time series, and \eqn{GHI} is measured irradiance.
}
\section{References}{

Global Horizontal Irradiance Clear Sky Models: Implementation and Analysis,
Reno et al, 2012, pp. 30.
}
\keyword{internal}

