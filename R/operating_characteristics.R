#' Calculate the Expected Number of Correct Decisions for a Basket Trial Design
#'
#' @template design
#' @template n
#' @template p1
#' @template lambda
#' @template design_params
#' @template iter
#' @template data
#' @template dotdotdot
#'
#' @return A numeric value.
#' @export
#'
#' @examples
#' # Example for a basket trial with Fujikawa's Design
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#' ecd(design = design, n = 20, p1 = c(0.2, 0.5, 0.5), lambda = 0.95,
#'   design_params = list(epsilon = 2, tau = 0), iter = 1000)
ecd <- function(design, n, p1, lambda, design_params = list(), iter = 1000,
                data = NULL, ...) {
  res <- do.call(get_results, args = c(design = list(design), n = n,
    p1 = list(p1), lambda = lambda, design_params, iter = iter,
    data = list(data), ...))
  targ <- design$p0 != p1
  mean(rowSums(t(apply(res, 1, function(x) x == targ))))
}

#' Calculate the Type 1 Error Rate for a Basket Trial Design
#'
#' @template design
#' @template n
#' @template p1_toer
#' @template lambda
#' @template design_params
#' @template iter
#' @template data
#' @template dotdotdot
#'
#' @return A numeric value.
#' @export
#'
#' @examples
#' # Example for a basket trial with Fujikawa's Design
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#' toer(design = design, n = 20, p1 = c(0.2, 0.5, 0.5), lambda = 0.95,
#'   design_params = list(epsilon = 2, tau = 0), iter = 1000)
toer <- function(design, n, p1 = NULL, lambda, design_params = list(),
                 iter = 1000, data = NULL, ...) {
  if (is.null(p1)) p1 <- rep(design$p0, design$k)
  res <- do.call(get_results, args = c(design = list(design), n = list(n),
                 p1 = list(p1), lambda = lambda, design_params,
                 iter = iter, data = list(data), ...))
  res_sel <- res[, p1 == design$p0, drop = FALSE]
  res_all <- apply(res_sel, 1, function(x) any(x == 1))
  mean(res_all)
}

#' Calculate the Power for a Basket Trial Design
#'
#' @template design
#' @template n
#' @template p1
#' @template lambda
#' @template design_params
#' @template iter
#' @template data
#' @template dotdotdot
#'
#' @return A numeric value.
#' @export
#'
#' @examples
#' # Example for a basket trial with Fujikawa's Design
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#' pow(design = design, n = 20, p1 = c(0.2, 0.5, 0.5), lambda = 0.95,
#'   design_params = list(epsilon = 2, tau = 0), iter = 1000)
pow <- function(design, n, p1 = NULL, lambda, design_params = list(),
                 iter = 1000, data = NULL, ...) {
  if (all(p1 == rep(design$p0, design$k))) stop("p1 can not be the global null hypothesis for calculating power")
  res <- do.call(get_results, args = c(design = list(design), n = list(n),
                                       p1 = list(p1), lambda = lambda, design_params,
                                       iter = list(iter), data = data, ...))
  res_sel <- res[, p1 != design$p0, drop = FALSE]
  res_all <- apply(res_sel, 1, function(x) any(x == 1))
  mean(res_all)
}

#' Calculate Power and Type I Error Rate for a Basket Trial Design
#'
#' @template design
#' @template n
#' @template p1_toer
#' @template lambda
#' @template design_params
#' @template iter
#' @template data
#' @template dotdotdot
#'
#' @return A numeric value.
#' @export
#'
#' @examples
#' # Example for a basket trial with Fujikawa's Design
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#' pow_toer(design = design, n = 20, p1 = c(0.2, 0.5, 0.5), lambda = 0.95,
#'   design_params = list(epsilon = 2, tau = 0), iter = 1000)
pow_toer <- function(design, n, p1 = NULL, lambda, design_params = list(),
                iter = 1000, data = NULL, ...) {
  if (is.null(p1)) p1 <- rep(design$p0, design$k)
  res <- do.call(get_results, args = c(design = list(design), n = list(n),
                                       p1 = list(p1), lambda = lambda, design_params,
                                       iter = list(iter), data = data, ...))
  res_sel_pow <- res[, p1 != design$p0, drop = FALSE]
  res_all_pow <- apply(res_sel_pow, 1, function(x) any(x == 1))
  res_sel_toer <- res[, p1 == design$p0, drop = FALSE]
  res_all_toer <- apply(res_sel_toer, 1, function(x) any(x == 1))
  return(c(FWER = mean(res_all_toer), EWP = mean(res_all_pow)))
}
