% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_details.R
\name{get_details.bma}
\alias{get_details.bma}
\title{Get Details of a BMA Basket Trial Simulation}
\usage{
\method{get_details}{bma}(design, n, p1, lambda, pmp0, iter = 1000, data = NULL, ...)
}
\arguments{
\item{design}{An object of class \code{bma}.}

\item{n}{The sample size per basket.}

\item{p1}{Probabilities under the alternative hypothesis.}

\item{lambda}{The posterior probability threshold.}

\item{pmp0}{Power parameter that is used to compute prior probabilities. See
\code{\link[bmabasket]{bma}} for details.}

\item{iter}{The number of iterations in the simulation.}

\item{data}{A data matrix with k column with the number of responses for each
basket. If \code{data} is used, then \code{iter} and \code{p1} are ignored.}

\item{...}{Further arguments.}
}
\value{
A list containing the rejection probabilites, posterior means,
and mean squared errors for all baskets.
}
\description{
Get Details of a BMA Basket Trial Simulation
}
\examples{
design <- setup_bma(k = 3, p0 = 0.2)
get_details(design = design, n = 20, p1 = 0.5, lambda = 0.95, pmp0 = 1,
  iter = 100)
}