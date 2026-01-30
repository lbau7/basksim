
<!-- README.md is generated from README.Rmd. Please edit that file -->

# basksim

<!-- badges: start -->

[![R-CMD-check](https://github.com/lbau7/basksim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lbau7/basksim/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/lbau7/basksim/branch/master/graph/badge.svg?token=AVO4V52BTH)](https://app.codecov.io/gh/lbau7/basksim)
[![CRAN
status](https://www.r-pkg.org/badges/version/basksim)](https://CRAN.R-project.org/package=basksim)
<!-- badges: end -->

## Overview

`basksim` calculates the operating characteristics of different basket
trial designs based on simulation.

## Installation

Install the development version with:

``` r
# install.packages("devtools")
devtools::install_github("lbau7/basksim")
```

## Usage

With `basksim` you can calculate the operating characteristics such as
rejection probabilities and mean squared error of single-stage basket
trials with different designs.

At first, you have to create a design-object using a setup-function. For
example to create a design-object for Fujikawa’s design (Fujikawa et
al., 2020):

``` r
library(basksim)
design <- setup_fujikawa(k = 3, shape1 = 1, shape2 = 1, p0 = 0.2)
```

`k` is the number of baskets, `shape1` and `shape2` are the shape
parameters of the Beta-prior of the response probabilities of each
baskets and `p0` is the response probability that defines the null
hypothesis.

Use `get_details` to estimate several important operating
characteristics:

``` r
set.seed(123)
get_details(
  design = design,
  n = c(15, 20, 25),
  p1 = c(0.2, 0.5, 0.5),
  lambda = 0.95,
  epsilon = 1.5,
  tau = 0,
  iter = 5000
)

# $Rejection_Probabilities
# [1] 0.4226 0.9824 0.9874
# 
# $FWER
# [1] 0.4226
# 
# $EWP
# [1] 0.999
# 
# $Mean
# [1] 0.2992626 0.4823250 0.4836304
# 
# $MSE
# [1] 0.020532553 0.007330251 0.006862607
# 
# $Lower_CL
# [1] 0.1517281 0.3407342 0.3440962
# 
# $Upper_CL
# [1] 0.4574680 0.6241900 0.6234426
# 
# $ECD
# [1] 2.5472
# 
# $Rejection_Probabilities_SE
# [1] 0.006985832 0.001859583 0.001577418
# 
# $FWER_SE
# [1] 0.006985832
# 
# $EWP_SE
# [1] 0.0004469899
# 
# $ECD_SE
# [1] 0.007147353
```
