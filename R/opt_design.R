#' Optimize a Basket Trial Design
#'
#' @template design
#' @template n
#' @template alpha
#' @template design_params
#' @param scenarios
#' @template prec_digits
#' @template iter
#' @template dotdotdot
#'
#' @return
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
  colnames(ecd_res) <- names(scenarios)
  lambdas <- numeric(lgrid)

  for (i in 1:lgrid) {
    params_loop <- lapply(as.list(grid), function(x) x[i])
    l <- do.call(adjust_lambda, args = c(design = list(design), n = n,
      p1 = NULL, alpha = alpha, params_loop, iter = iter,
      prec_digits = prec_digits, ...))
    lambdas[i] <- l$lambda

    for (j in 1:ncol(scenarios)) {
      ecd_res[i, j] <- do.call(ecd, args = c(design = list(design), n = n,
        p1 = list(scenarios[, i]), lambda = l$lambda, design_params,
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
