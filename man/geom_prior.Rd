% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geoms.R
\name{geom_prior}
\alias{geom_prior}
\title{Plot a Bayesian basket trial's prior distribution}
\usage{
geom_prior(design, ...)
}
\arguments{
\item{design}{An object created with one of the \code{setup} functions.}

\item{...}{Further arguments to be passed to `geom_function`.}
}
\value{
A list of ggplot layers of type `geom_function`.
}
\description{
Plot a Bayesian basket trial's prior distribution
}
\examples{
# Example for a basket trial with Fujikawa's Design
design <- setup_fujikawa(k = 3, p0 = 0.2)
# One facet per basket
library(ggplot2)
ggplot() +
    geom_prior(design) +
    facet_wrap(vars(basket))
# Colour different baskets
ggplot() +
    geom_prior(design, aes(colour = basket))
}
