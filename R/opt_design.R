#' Optimize a Basket Trial Design
#'
#' @template design
#' @template n
#' @template alpha
#' @template design_params
#' @param scenarios A matrix of scenarios.
#' @template prec_digits
#' @template iter
#' @template dotdotdot
#'
#' @return A matrix with the expected number of correct decisions.
#' @export
#'
#' @examples
opt_design <- function(design, n, alpha, design_params = list(), scenarios,
                       prec_digits, iter = 1000, ...) {
  grid <- expand.grid(design_params)
  if (length(design_params) == 0) {
    lgrid <- 1
  } else {
    lgrid <- nrow(grid)
  }

  ecd_res <- matrix(nrow = lgrid, ncol = ncol(scenarios))
  colnames(ecd_res) <- colnames(scenarios)
  lambdas <- numeric(lgrid)

  for (i in 1:lgrid) {
    params_loop <- lapply(as.list(grid), function(x) x[i])
    l <- do.call(adjust_lambda, args = c(design = list(design), n = n,
      p1 = NULL, alpha = alpha, params_loop, iter = iter,
      prec_digits = prec_digits, ...))
    lambdas[i] <- l$lambda

    for (j in 1:ncol(scenarios)) {
      ecd_res[i, j] <- do.call(ecd, args = c(design = list(design), n = n,
        p1 = list(scenarios[, j]), lambda = l$lambda, params_loop,
        iter = iter, data = NULL, ...))
    }
  }

  if (ncol(grid) == 0) {
    cbind(lambdas, ecd_res, "Mean_ECD" = rowMeans(ecd_res))
  } else {
    ecd_res <- cbind(grid, "Lambda" = lambdas, ecd_res,
      "Mean_ECD" = rowMeans(ecd_res))
    ecd_res[order(ecd_res[, ncol(ecd_res)], decreasing = TRUE), ]
  }
}

#' Create a Scenario Matrix
#'
#' Creates a default scenario matrix.
#'
#' @template design
#' @param theta1 Probabilitiy under the alternative hypothesis.
#'
#' @details \code{get_scenarios} creates a default scenario matrix
#' that can be used for \code{\link{opt_design}}. The function creates
#' \code{k + 1} scenarios, from a global null to a global alternative scenario.
#'
#' @return A matrix with \code{k} rows and \code{k + 1} columns.
#' @export
#'
#' @examples
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#' get_scenarios(design = design, theta1 = 0.5)
get_scenarios <- function(design, theta1) {
  scen_mat <- matrix(nrow = design$k, ncol = design$k + 1)
  for (i in 0:design$k) {
    scen_mat[, (i + 1)] <- c(rep(design$p0, design$k - i),
      rep(theta1, i))
  }
  colnames(scen_mat) <- paste(0:design$k, "Active")
  scen_mat
}
