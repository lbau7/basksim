#' Adjust Lambda
#'
#' @template design
#' @template n
#' @template p1_toer
#' @template alpha
#' @template design_params
#' @template iter
#' @template prec_digits
#' @template dotdotdot
#' @template data
#'
#' @details It is recommended to use \code{data} and then use the same simulated
#' data set for all further calculations. If \code{data = NULL} then
#' new data is generated in each step of the algorithm, so \code{lambda} doesn't
#' necessarily protect the family wise error rate for different simulated data
#' due to Monte Carlo simulation error.
#'
#' @return A list containing the greatest estimated value for \code{lambda} with
#' \code{prec_digits} decimal places which controls the family wise error rate
#' at level \code{alpha} (one-sided) and the estimated family wise error rate
#' for the estimated \code{lambda}.
#' @export
#'
#' @examples
#' # Example for a basket trial with Fujikawa's Design
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#' adjust_lambda(design = design, n = 20, alpha = 0.05,
#'   design_params = list(epsilon = 2, tau = 0), iter = 1000)
adjust_lambda <- function(design, n, p1 = NULL, alpha = 0.05,
                          design_params = list(), iter = 1000, prec_digits = 3,
                          data = NULL, ...) {
  upper_lim <- 1 - 10^(-prec_digits)
  root_fun <- function(x) do.call(toer, args = c(design = list(design),
    n = n, p1 = list(p1), lambda = x, design_params, iter = iter,
    data = list(data),
    ...)) - alpha

  # Use uniroot to find lambda close to the smallest lambda that protects
  # the significance level at alpha
  uni_root <- stats::uniroot(root_fun, interval = c(0.5, upper_lim),
    tol = 10^(-prec_digits))

  if (uni_root$f.root > 0 ) {
    # If rejection prob is greater than alpha after uniroot, round lambda up
    root <- ceiling(uni_root$root * 10^(prec_digits)) / 10^(prec_digits)
    val <- root_fun(root)
    if (val > 0) {
      # If rejection prob is still above alpha, increase lambda
      while (val > 0) {
        root <- root + 10^(-prec_digits)
        val <- root_fun(root)
      }
    }
  } else {
    # If rejection prob is less than alpha after uniroot, round lambda down
    root <- floor(uni_root$root * 10^(prec_digits)) / 10^(prec_digits)
    val <- root_fun(root)
    if (val > 0) {
      # If the rejection prob is greater now than alpha with the rounded-down
      # lambda, then round lambda up
      root <- ceiling(uni_root$root * 10^(prec_digits)) / 10^(prec_digits)
      val <- root_fun(root)
    } else {
      # If the rejection prob is still below alpha, decrease lambda
      repeat {
        root_old <- root
        val_old <- val
        root <- root - 10^(-prec_digits)
        val <- root_fun(root)
        if (val > 0) {
          root <- root_old
          val <- val_old
          break
        }
      }
    }
  }
  return(list(
    lambda = root,
    toer = val + alpha
  ))
}
