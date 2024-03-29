% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_results.R
\name{get_results}
\alias{get_results}
\title{Get Results for Simulation of Basket Trial Designs}
\usage{
get_results(design, ...)
}
\arguments{
\item{design}{An object created with one of the \code{setup} functions.}

\item{...}{Further arguments.}
}
\value{
A matrix of results with \code{iter} rows. A 0 means, that the
null hypothesis that the response probability exceeds \code{p0} was not
rejected, a 1 means, that the null hypothesis was rejected.
}
\description{
Get Results for Simulation of Basket Trial Designs
}
\examples{
# Example for a basket trial with Fujikawa's Design
design <- setup_fujikawa(k = 3, p0 = 0.2)
get_results(design = design, n = 20, p1 = c(0.2, 0.5, 0.5), lambda = 0.95,
  epsilon = 2, tau = 0, iter = 100)
}
