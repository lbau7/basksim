#' Evaluate a Basket Trial
#'
#' @template design
#' @template dotdotdot
#'
#' @return Posterior probabilities and the point estimates of the response rates for all baskets.
#' @export
#'
#' @examples
#' #
#'
get_evaluation <- function(design, ...) {
  UseMethod("get_evaluation", design)
}



#' Evaluate a BMA Basket Trial
#'
#' @template design_bma
#' @template n
#' @template r
#' @template lambda
#' @template pmp0
#' @template dotdotdot
#'
#' @return Posterior probabilities and the point estimates of the response rates for all baskets.
#' @export
#'
#' @examples
#' design <- setup_bma(k = 3, p0 = 0.2)
#'
get_evaluation.bma <- function(design, n, r, lambda, pmp0, ...) {

  # n must be passed in the correct form
  if((length(n) < design$k & length(n) != 1) | length(n) > design$k){
    stop("n must either have length 1 or k")
  }

  if(length(n) == 1){
    n <- rep(n, design$k)
  }

  res <- bmabasket::bma(pi0 = design$p0, y = r, n = n, pmp0 = pmp0)

  list(
    Posterior_Probabilities = as.vector(res$bmaProbs),
    Estimates = as.vector(res$bmaMeans)
  )

}



#' Evaluate a BHM Basket Trial
#'
#' @template design_bhm
#' @template n
#' @template r
#' @template lambda
#' @template level
#' @template tau_bhm
#' @template iter
#' @template n_mcmc
#' @template dotdotdot
#'
#' @return Posterior probabilities and the point estimates of the response rates for all baskets.
#' @export
#'
#' @examples
#' design <- setup_bhm(k = 3, p0 = 0.2, p_target = 0.5)
#'
get_evaluation.bhm <- function(design, n, r, lambda, level = 0.95,
                            tau_scale, n_mcmc = 10000, ...) {

  # n must be passed in the correct form
  if((length(n) < design$k & length(n) != 1) | length(n) > design$k){
    stop("n must either have length 1 or k")
  }


  # data <- check_data_bhmbasket(data = data, design = design, n = n, p = p1,
  #                              iter = iter)

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

  br <- paste0("c(", paste0("x[", 1:design$k, "] > ", design$p0,
                            collapse = ", "), ")")

  res <- bhmbasket::getGoDecisions(
    analyses_list = analyses,
    cohort_names = paste("p", 1:design$k, sep = "_"),
    evidence_levels = rep(lambda, design$k),
    boundary_rules = str2lang(br)
  )$scenario_1$decisions_list$berry[, -1]   # hier stattdessen posterior Wahrscheinlichkeiten speichern!

  est <- bhmbasket::getEstimates(analyses, point_estimator = "mean",
                                 alpha_level = (1 - level))$berry

  list(
    # Rejection_Probabilities = unname(colMeans(res)),
    # FWER = mean(apply(res, 1, function(x) any(x[targ] == 1))),
    Estimates = unname(est[, 1])#,
    # MSE = unname(est[, 7]),
    # Lower_CL = unname(est[, 3]),
    # Upper_CL = unname(est[, 5])
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
#' @return Posterior probabilities and the point estimates of the response rates for all baskets.
#' @export
#'
#' @examples
#' design <- setup_exnex(k = 3, p0 = 0.2)
#'
get_evaluation.exnex <- function(design, n, r, lambda, level = 0.95,
                              tau_scale, w, n_mcmc = 10000, ...) {

  # n must be passed in the correct form
  if((length(n) < design$k & length(n) != 1) | length(n) > design$k){
    stop("n must either have length 1 or k")
  }

  # data <- check_data_bhmbasket(data = data, design = design, n = n, p = p1,
  #                              iter = iter)

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

  br <- paste0("c(", paste0("x[", 1:design$k, "] > ", design$p0,
                            collapse = ", "), ")")
  res <- bhmbasket::getGoDecisions(
    analyses_list = analyses,
    cohort_names = paste("p", 1:design$k, sep = "_"),
    evidence_levels = rep(lambda, design$k),
    boundary_rules = str2lang(br)
  )$scenario_1$decisions_list$exnex[, -1]    # hier stattdessen posterior Wahrscheinlichkeiten speichern!

  est <- bhmbasket::getEstimates(analyses, point_estimator = "mean",
                                 alpha_level = (1 - level))$exnex

  list(
    # Rejection_Probabilities = unname(colMeans(res)),
    # FWER = mean(apply(res, 1, function(x) any(x[targ] == 1))),
    Estimates = unname(est[, 1])#,
    # MSE = unname(est[, 7]),
    # Lower_CL = unname(est[, 3]),
    # Upper_CL = unname(est[, 5])
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
#' @return Posterior probabilities and the point estimates of the response rates for all baskets.
#' @export
#'
#' @examples
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#'
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
#' @return Posterior probabilities and the point estimates of the response rates for all baskets.
#' @export
#'
#' @examples
#' design <- setup_cpp(k = 3, p0 = 0.2)
#'
get_evaluation.cpp <- function(design, n, r, lambda, level = 0.95,
                            tune_a, tune_b, ...) {
  # n must be passed in the correct form
  if((length(n) < design$k & length(n) != 1) | length(n) > design$k){
    stop("n must either have length 1 or k")
  }

  weights <- get_weights_cpp(n = n, tune_a = tune_a, tune_b = tune_b)

  shape <- beta_borrow_cpp(design = design, n = n, r = r, weights = weights)
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
#' @return Posterior probabilities and the point estimates of the response rates for all baskets.
#' @export
#'
#' @examples
#' design <- setup_cpplim(k = 3, p0 = 0.2)
#'
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
#' @return Posterior probabilities and the point estimates of the response rates for all baskets.
#' @export
#'
#' @examples
#' design <- setup_app(k = 3, p0 = 0.2)
#'
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




