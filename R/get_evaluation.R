#' Evaluate a Basket Trial
#'
#' @template design
#' @template dotdotdot
#'
#' @return A list containing the point estimates of the basket-specific response rates and,
#' for some methods, the posterior probabilities that the estimated response rates are above
#' a specified threshold p0.
#' @export
#'
#' @examples
#' # Example for a basket trial with Fujikawa's Design
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#'
#' # Equal sample sizes
#' get_evaluation(design = design, n = 20, r = c(10, 15, 5),
#'   lambda = 0.95, epsilon = 2, tau = 0, iter = 100)
#'
#' # Unequal sample sizes
#' get_evaluation(design = design, n = c(15, 20, 25),
#'    r = c(10, 15, 17), lambda = 0.95, epsilon = 2,
#'    tau = 0, iter = 100)
#'
get_evaluation <- function(design, ...) {
  UseMethod("get_evaluation", design)
}





#' Evaluate a BHM Basket Trial
#'
#' @template design_bhm
#' @template n
#' @template r
#' @template lambda
#' @template level
#' @template tau_bhm
#' @template n_mcmc
#' @template dotdotdot
#'
#' @return A list containing the point estimates of the basket-specific response rates.
#' @export
#'
#' @examples
#' design <- setup_bhm(k = 3, p0 = 0.2, p_target = 0.5)
#'
#' get_evaluation(design = design, n = c(20, 20, 20), r = c(10, 15, 5),
#'   lambda = 0.95, tau_scale = 1, iter = 100)
#'
#' # Unequal sample sizes
#' get_evaluation(design = design, n = c(15, 20, 25), r = c(10, 15, 17),
#'   lambda = 0.95, tau_scale = 1, iter = 100)
get_evaluation.bhm <- function(design, n, r, lambda, level = 0.95,
                            tau_scale, n_mcmc = 10000, ...) {

  # n must be passed in the correct form
  if((length(n) < design$k & length(n) != 1) | length(n) > design$k){
    stop("n must either have length 1 or k")
  }

  analyses <- suppressMessages(bhmbasket::performAnalyses(
    scenario_list = bhmbasket::createTrial(n_subjects = n, n_responders = r),
    evidence_levels = c(lambda, 1 - level),
    method_names = "berry",
    target_rates = rep(design$p_target, design$k),
    prior_parameters_list = bhmbasket::setPriorParametersBerry(
      mu_mean = design$mu_mean,
      mu_sd = design$mu_sd,
      tau_scale = tau_scale
    ),
    n_mcmc_iterations = n_mcmc
  ))

  est <- bhmbasket::getEstimates(analyses, point_estimator = "mean",
                                 alpha_level = (1 - level))$berry

  list(
    Estimates = unname(est[, 1])
  )
}




#' Evaluate a Basket Trial with the EXNEX Design
#'
#' @template design_exnex
#' @template n
#' @template r
#' @template lambda
#' @template level
#' @template tau_exnex
#' @template w_exnex
#' @template n_mcmc
#' @template dotdotdot
#'
#' @return A list containing the point estimates of the basket-specific response rates.
#' @export
#'
#' @examples
#' design <- setup_exnex(k = 3, p0 = 0.2)
#'
#' # Equal sample sizes
#' get_evaluation(design = design, n = c(20, 20, 20), r = c(10, 15, 5),
#'   lambda = 0.95, tau_scale = 1, w = 0.5, iter = 100)
#'
#' # Unequal sample sizes
#' get_evaluation(design = design, n = c(15, 20, 25), r = c(10, 15, 17),
#'   lambda = 0.95, tau_scale = 1, w = 0.5, iter = 100)
get_evaluation.exnex <- function(design, n, r, lambda, level = 0.95,
                              tau_scale, w, n_mcmc = 10000, ...) {

  # n must be passed in the correct form
  if((length(n) < design$k & length(n) != 1) | length(n) > design$k){
    stop("n must either have length 1 or k")
  }

  analyses <- suppressMessages(bhmbasket::performAnalyses(
    scenario_list = bhmbasket::createTrial(n_subjects = n, n_responders = r),
    evidence_levels = c(lambda, 1 - level),
    method_names = "exnex",
    prior_parameters_list = bhmbasket::setPriorParametersExNex(
      mu_mean = design$mu_mean,
      mu_sd = design$mu_sd,
      tau_scale = tau_scale,
      mu_j = rep(design$basket_mean, design$k),
      tau_j = rep(design$basket_sd, design$k),
      w_j = w
    ),
    n_mcmc_iterations = n_mcmc
  ))

  est <- bhmbasket::getEstimates(analyses, point_estimator = "mean",
                                 alpha_level = (1 - level))$exnex

  list(
    Estimates = unname(est[, 1])
  )
}




#' Evaluate a Basket Trial with Fujikawa's Design
#'
#' @template design_fujikawa
#' @template n
#' @template r
#' @template lambda
#' @template level
#' @template tuning_fujikawa
#' @template dotdotdot
#'
#' @return A list containing the point estimates of the basket-specific response rates and the
#' posterior probabilities that the estimated response rates are above a specified threshold p0.
#' @export
#'
#' @examples
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#'
#' # Equal sample sizes
#' get_evaluation(design = design, n = 20, r = c(10, 15, 5),
#'   lambda = 0.95, epsilon = 2, tau = 0, iter = 100)
#'
#' # Unequal sample sizes
#' get_evaluation(design = design, n = c(15, 20, 25),
#'    r = c(10, 15, 17), lambda = 0.95, epsilon = 2,
#'    tau = 0, iter = 100)
get_evaluation.fujikawa <- function(design, n, r, lambda, level = 0.95,
                                 epsilon, tau, logbase = 2, ...) {
  # n must be passed in the correct form
  if((length(n) < design$k & length(n) != 1) | length(n) > design$k){
    stop("n must either have length 1 or k")
  }
  weights <- get_weights_jsd(design = design, n = n, epsilon = epsilon,
                             tau = tau, logbase = logbase)

  shape <- beta_borrow_fujikawa(design = design, n = n, r = r, weights = weights)
  posterior <- post_beta(shape, design$p0)
  estimates <- apply(shape, 2, function(x) x[1] / (x[1] + x[2]))


  list(
    Posterior_Probabilities = posterior,
    Estimates = estimates
  )
}




#' Evaluate a Basket Trial with the Calibrated Power Prior
#' Design
#'
#' @template design_cpp
#' @template n
#' @template r
#' @template lambda
#' @template level
#' @template tuning_cpp
#' @template dotdotdot
#'
#' @return A list containing the point estimates of the basket-specific response rates and the
#' posterior probabilities that the estimated response rates are above a specified threshold p0.
#' @export
#'
#' @examples
#' design <- setup_cpp(k = 3, p0 = 0.2)
#'
#' # Equal sample sizes
#' get_evaluation(design = design, n = 20, r = c(10, 15, 5),
#'   lambda = 0.95, tune_a = 1, tune_b = 1, iter = 100)
#'
#' # Unequal sample sizes
#' get_evaluation(design = design, n = c(15, 20, 25), r = c(10, 15, 17),
#'   lambda = 0.95, tune_a = 1, tune_b = 1, iter = 100)
get_evaluation.cpp <- function(design, n, r, lambda, level = 0.95,
                            tune_a, tune_b, ...) {
  # n must be passed in the correct form
  if((length(n) < design$k & length(n) != 1) | length(n) > design$k){
    stop("n must either have length 1 or k")
  }

  weights <- get_weights_cpp(n = n, tune_a = tune_a, tune_b = tune_b)

  shape <- beta_borrow_pp(design = design, n = n, r = r, weights = weights)
  posterior <- post_beta(shape, design$p0)
  estimates <- apply(shape, 2, function(x) x[1] / (x[1] + x[2]))


  list(
    Posterior_Probabilities = posterior,
    Estimates = estimates
  )
}




#' Evaluate a Basket Trial with the Limited Calibrated Power
#' Prior Design
#'
#' @template design_cpplim
#' @template n
#' @template r
#' @template lambda
#' @template level
#' @template tuning_cpp
#' @template dotdotdot
#'
#' @return A list containing the point estimates of the basket-specific response rates and the
#' posterior probabilities that the estimated response rates are above a specified threshold p0.
#' @export
#'
#' @examples
#' design <- setup_cpplim(k = 3, p0 = 0.2)
#'
#' # Equal sample sizes
#' get_evaluation(design = design, n = 20, r = c(10, 15, 5),
#'   lambda = 0.95, tune_a = 1, tune_b = 1, iter = 100)
#'
#' # Unequal sample sizes
#' get_evaluation(design = design, n = c(15, 20, 25), r = c(10, 15, 17),
#'   lambda = 0.95, tune_a = 1, tune_b = 1, iter = 100)
get_evaluation.cpplim <- function(design, n, r, lambda, level = 0.95,
                               tune_a, tune_b, ...) {

  # n must be passed in the correct form
  if((length(n) < design$k & length(n) != 1) | length(n) > design$k){
    stop("n must either have length 1 or k")
  }

  weights <- get_weights_cpp(n = n, tune_a = tune_a, tune_b = tune_b)

  alpha_0 <- get_alpha_0_app(design = design, n = n)

  shape <- beta_borrow_cpplim(design = design, n = n, r = r,
                              weights = weights, alpha_0 = alpha_0)
  posterior <- post_beta(shape, design$p0)
  estimates <- apply(shape, 2, function(x) x[1] / (x[1] + x[2]))


  list(
    Posterior_Probabilities = posterior,
    Estimates = estimates
  )

}





#' Evaluate a Basket Trial with the Adaptive Power Prior Design
#' for sequential clinical trials
#'
#' @template design_app
#' @template n
#' @template r
#' @template lambda
#' @template level
#' @template dotdotdot
#'
#' @return A list containing the point estimates of the basket-specific response rates and the
#' posterior probabilities that the estimated response rates are above a specified threshold p0.
#' @export
#'
#' @examples
#' design <- setup_app(k = 3, p0 = 0.2)
#'
#' # Equal sample sizes
#' get_evaluation(design = design, n = 20, r = c(10, 15, 5),
#'  lambda = 0.95, iter = 100)
#'
#' # Unequal sample sizes
#' get_evaluation(design = design, n = c(15, 20, 25), r = c(10, 15, 17),
#'  lambda = 0.95, iter = 100)
#'
get_evaluation.app <- function(design, n, r, lambda, level = 0.95, ...) {

  # n must be passed in the correct form
  if((length(n) < design$k & length(n) != 1) | length(n) > design$k){
    stop("n must either have length 1 or k")
  }

  alpha_0 <- get_alpha_0_app(design = design, n = n)

  shape <- beta_borrow_app(design = design, n = n, r = r, alpha_0 = alpha_0)
  posterior <- post_beta(shape, design$p0)
  estimates <- apply(shape, 2, function(x) x[1] / (x[1] + x[2]))


  list(
    Posterior_Probabilities = posterior,
    Estimates = estimates
  )

}




