test_that("get_details works for bhm", {
  set.seed(1)
  scenarios <- bhmbasket::simulateScenarios(
    n_subjects_list = list(rep(10, 3)),
    response_rates_list = list(c(0.15, 0.5, 0.5)),
    n_trials = 100
  )

  design <- setup_bhm(k = 3, p0 = 0.15, p_target = 0.5,
    mu_mean = -1.7346, mu_sd = 100)

  set.seed(20230515)
  res1 <- get_details(design = design, n = 10, p1 = c(0.15, 0.5, 0.5),
    lambda = 0.9, tau_scale = 0.75, iter = 100, data = scenarios)

  # Results with bhmbasket
  set.seed(20230515)
  ana <- suppressMessages(bhmbasket::performAnalyses(
    scenario_list = scenarios,
    evidence_levels = 0.9,
    method_names = "berry",
    target_rates = rep(0.5, 3),
    prior_parameter_list <- bhmbasket::setPriorParametersBerry(
      mu_mean = -1.7346,
      mu_sd = 100,
      tau_scale = 0.75
    )
  ))

  godec <- bhmbasket::getGoDecisions(
    analyses_list = ana,
    cohort_names = paste("p", 1:3, sep = "_"),
    evidence_levels = rep(0.9, 3),
    boundary_rules = quote(c(x[1] > 0.15, x[2] > 0.15, x[3] > 0.15))
  )

  res2 <- bhmbasket::getGoProbabilities(godec)

  estim <- bhmbasket::getEstimates(
    analyses_list = ana,
    point_estimator = "mean",
    alpha_level = 0.05
  )

  # Results are equal with get_details and bhmbasket
  expect_equal(res1$Rejection_Probabilities, unname(unlist(res2))[-1])
  expect_equal(res1$Mean, unname(estim$berry[, 1]))
  expect_equal(res1$MSE, unname(estim$berry[, 7]))
  expect_equal(res1$Lower_CL, unname(estim$berry[, 3]))
  expect_equal(res1$Upper_CL, unname(estim$berry[, 5]))

  # Works without supplied data
  res3 <- get_details(design = design, n = 10, p1 = c(0.15, 0.5, 0.5),
    lambda = 0.9, tau_scale = 0.75, iter = 100)
  expect_equal(length(res3), 7)
  expect_equal(res3$Rejection_Probabilities[1], res3$FWER)

  # Error works
  data <- get_data(k = 3, n = 20, p = 0.5, iter = 100)
  expect_error(get_details(design, n = 20, p1 = c(0.2, 0.2, 0.5), lambda = 0.95,
    tau_scale = 1, iter = 100, data = data))

  # If n is passed as a vector, it should have k entries
  expect_error(get_details.bhm(design = design, n = c(10,20), p1 = NULL,
                               lambda = 0.95, pmp0 = 1, data = NULL, iter = 110))
  expect_error(get_details.bhm(design = design, n = c(10,20,30,40), p1 = NULL,
                               lambda = 0.95, pmp0 = 1, data = NULL, iter = 110))

  # Works without supplied p1
  expect_no_error(get_details(design = design, n = 10, p1 = NULL, lambda = 0.9,
    tau_scale = 0.75, iter = 100))
})

test_that("get_details works for exnex", {
  scenarios <- bhmbasket::simulateScenarios(
    n_subjects_list = list(rep(10, 3)),
    response_rates_list = list(c(0.15, 0.5, 0.5)),
    n_trials = 100
  )

  design <- setup_exnex(k = 3, p0 = 0.15)

  set.seed(20230515)
  res1 <- get_details(design = design, n = 10, p1 = c(0.15, 0.5, 0.5),
    lambda = 0.9, tau_scale = 0.75, w = 0.5, iter = 100, data = scenarios)

  # Results with bhmbasket
  set.seed(20230515)
  ana <- suppressMessages(bhmbasket::performAnalyses(
    scenario_list = scenarios,
    evidence_levels = 0.9,
    method_names = "exnex",
    target_rates = NULL,
    prior_parameter_list <- bhmbasket::setPriorParametersExNex(
      mu_mean = bhmbasket:::logit(0.15),
      mu_sd = 100,
      tau_scale = 0.75,
      mu_j = rep(bhmbasket:::logit(0.15), 3),
      tau_j = rep(100, 3),
      w = 0.5
    )
  ))

  godec <- bhmbasket::getGoDecisions(
    analyses_list = ana,
    cohort_names = paste("p", 1:3, sep = "_"),
    evidence_levels = rep(0.9, 3),
    boundary_rules = quote(c(x[1] > 0.15, x[2] > 0.15, x[3] > 0.15))
  )

  res2 <- bhmbasket::getGoProbabilities(godec)

  estim <- bhmbasket::getEstimates(
    analyses_list = ana,
    point_estimator = "mean",
    alpha_level = 0.05
  )

  # Results are equal with get_details and bhmbasket
  expect_equal(res1$Rejection_Probabilities, unname(unlist(res2))[-1])
  expect_equal(res1$Mean, unname(estim$exnex[, 1]))
  expect_equal(res1$MSE, unname(estim$exnex[, 7]))
  expect_equal(res1$Lower_CL, unname(estim$exnex[, 3]))
  expect_equal(res1$Upper_CL, unname(estim$exnex[, 5]))

  # Works without supplied data
  res3 <- get_details(design = design, n = 10, p1 = c(0.15, 0.5, 0.5),
    lambda = 0.9, tau_scale = 0.75, w = 0.5, iter = 100)
  expect_equal(length(res3), 7)
  expect_equal(res3$Rejection_Probabilities[1], res3$FWER)

  # Errors work
  data <- get_data(k = 3, n = 20, p = 0.5, iter = 100)
  expect_error(get_details(design, n = 20, p1 = c(0.2, 0.2, 0.5), lambda = 0.95,
    tau_scale = 1, w = 0.5, iter = 100, data = data))
  expect_error(get_details(design, n = 20, p1 = 0.2, lambda = 0.95,
    tau_scale = 1, w = 0.5, iter = 100, data = scenarios))

  # If n is passed as a vector, it should have k entries
  expect_error(get_details.exnex(design = design, n = c(10,20), p1 = NULL,
                               lambda = 0.95, pmp0 = 1, data = NULL, iter = 110))
  expect_error(get_details.exnex(design = design, n = c(10,20,30,40), p1 = NULL,
                               lambda = 0.95, pmp0 = 1, data = NULL, iter = 110))

  # Works without supplied p1
  expect_no_error(get_details(design = design, n = 10, p1 = NULL, lambda = 0.9,
    tau_scale = 0.75, w = 0.5, iter = 100))
})


test_that("get_details works for fujikawa", {
  # With 3 baskets
  set.seed(20230319)
  design1 <- setup_fujikawa(k = 3, p0 = 0.2)
  res1 <- get_details(design = design1, n = 15, p1 = c(0.2, 0.2, 0.5),
                      lambda = 0.99, epsilon = 2, logbase = exp(1), tau = 0, iter = 5000)

  # Works without supplied p1
  expect_no_error(get_details(design = design, n = 15, p1 = NULL, lambda = 0.99,
    epsilon = 2, logbase = exp(1), tau = 0, iter = 5000))

  # Compare with results from baskexact
  expect_true(all(abs(res1$Rejection_Probabilities -
                        c(0.1003108, 0.1003108, 0.5965844)) < 0.01))
  expect_true(all(abs(res1$Mean - c(0.2717930 , 0.2717930 , 0.4145559)) < 0.01))
  expect_true(all(abs(res1$MSE -
                        c(0.01043607, 0.01043607, 0.01674920)) < 0.01))
  expect_equal(res1$EWP, 0.5965844, tolerance = 0.01)
  expect_equal(res1$FWER, 0.1480935, tolerance = 0.01)

  # If n is passed as a vector, it should have k entries
  expect_error(get_details.fujikawa(design = design, n = c(10,20), p1 = NULL,
                                    lambda = 0.95, pmp0 = 1, data = NULL, iter = 110))
  expect_error(get_details.fujikawa(design = design, n = c(10,20,30,40), p1 = NULL,
                                    lambda = 0.95, pmp0 = 1, data = NULL, iter = 110))

  # With 4 baskets
  set.seed(20240308)
  design2 <- setup_fujikawa(k = 4, p0 = 0.15)
  res2 <- get_details(design = design2, n = 15, p1 = c(0.15, 0.3, 0.5, 0.15),
                      lambda = 0.95, epsilon = 2, logbase = 2, tau = 0.1, iter = 5000)

  # Compare with results from baskexact
  expect_true(all(abs(res2$Rejection_Probabilities -
                        c(0.3133054, 0.7274739, 0.9793341, 0.3133054)) < 0.01))
  expect_true(abs(res2$FWER - 0.4696476) < 0.01)
  expect_true(abs(res2$EWP - 0.9850177) < 0.01)
  expect_true(all(abs(res2$Mean -
                        c(0.2145436, 0.3134261, 0.4571671, 0.2145436)) < 0.01))
  expect_true(all(abs(res2$MSE -
                        c(0.009882112, 0.009797917, 0.016591157, 0.009882112)) < 0.01))
  expect_true(abs(res2$ECD - 3.080197) < 0.01)

  # Check MCSEs
  expect_equal(res2$FWER_SE, sqrt( (res2$FWER) * (1 - res2$FWER) / 5000))
  expect_equal(res2$EWP_SE, sqrt( (res2$EWP) * (1 - res2$EWP) / 5000))
  expect_equal(res2$Rejection_Probabilities_SE,
               sqrt( (res2$Rejection_Probabilities) *
                       (1 - res2$Rejection_Probabilities) / 5000))
  expect_true(res2$ECD_SE < 1/sqrt(5000))
  expect_true(res2$ECD_SE > 0.001/sqrt(5000))
})

test_that("switching off parallelization does not change the results of
          get_details.fujikawa()", {
            # With 4 baskets
            design <- setup_fujikawa(k = 4, p0 = 0.15)
            set.seed(20240308)
            res1 <- get_details(design = design, n = 15, p1 = c(0.15, 0.3, 0.5, 0.15),
                                lambda = 0.95, epsilon = 2, logbase = 2, tau = 0.1,
                                iter = 5000, use_future = TRUE)
            set.seed(20240308)
            res2 <- get_details(design = design, n = 15, p1 = c(0.15, 0.3, 0.5, 0.15),
                                lambda = 0.95, epsilon = 2, logbase = 2, tau = 0.1,
                                iter = 5000, use_future = FALSE)

            # Compare with results from baskexact
            expect_equal(res1$Rejection_Probabilities, res2$Rejection_Probabilities)
            expect_equal(res1$FWER, res2$FWER)
            expect_equal(res1$EWP, res2$EWP)
            expect_equal(res1$Mean, res2$Mean)
            expect_equal(res1$MSE, res2$MSE)
            expect_equal(res1$ECD, res2$ECD)
          })

test_that("get_details works for fujikawa with custom weights functions", {
  # With 3 baskets
  set.seed(20230319)
  weight_noshare <- function(design, n, epsilon, tau, logbase){
    n_sum <- n + 1
    return(diag(n_sum))
  }
  design1 <- setup_fujikawa(k = 3, p0 = 0.2)
  n <- 15
  p1 <- c(0.2, 0.5, 0.5)
  lambda <- 0.99
  logbase <- exp(1)
  tau <- 0
  epsilon <- 2
  iter <- 5000
  res <- get_details(design = design1, n = n, p1 = p1,
                     lambda = lambda, epsilon = epsilon, logbase = logbase,
                     tau = tau, iter = iter)
  rescustom <- get_details(design = design1, n = n, p1 = p1,
                           lambda = lambda, epsilon = epsilon, logbase = logbase,
                           tau = tau, iter = iter, weight_fun = weight_noshare)
  # Compare with results with sharing - sharing should increase power and TOER
  expect_true(all(rescustom$Rejection_Probabilities <
                    res$Rejection_Probabilities))
  expect_true(rescustom$FWER < res$FWER)
})

test_that("get_details works for jsdglobal", {
  # With 3 baskets
  set.seed(20230515)
  design1 <- setup_jsdglobal(k = 3, p0 = 0.2)
  res1 <- get_details(design = design1, n = 12, p1 = c(0.2, 0.5, 0.6),
                      lambda = 0.95, eps_pair = 1.5, eps_all = 0, iter = 1000)

  # Compare with results from baskexact
  expect_true(all(abs(res1$Rejection_Probabilities -
                        c(0.3500560, 0.8917081, 0.9701317)) < 0.01))
  expect_true(all(abs(res1$Mean - c(0.2976754, 0.4857321, 0.5432103)) < 0.01))
  expect_true(all(abs(res1$MSE -
                        c(0.02132311, 0.01401632, 0.01667987)) < 0.01))

  # With 4 baskets
  set.seed(20240308)
  design2 <- setup_jsdglobal(k = 4, p0 = 0.15)
  res2 <- get_details(design = design2, n = 12, p1 = c(0.15, 0.4, 0.3, 0.5),
                      lambda = 0.99, eps_pair = 1.5, tau = 0.2, eps_all = 0, iter = 1000)

  # Compare with results from baskexact
  expect_true(all(abs(res2$Rejection_Probabilities -
                        c(0.2070795, 0.7676506, 0.5765870, 0.8949090)) < 0.025))
  expect_true(all(abs(res2$Mean -
                        c(0.2374997, 0.3846106, 0.3261889, 0.4454463)) < 0.01))
  expect_true(all(abs(res2$MSE -
                        c(0.01581763, 0.01224553, 0.01111166, 0.01745138)) < 0.01))
  expect_true(abs(res2$ECD - 3.032067) < 0.025)
})

test_that("get_details works for cpp", {
  # With 3 baskets
  set.seed(20230319)
  design1 <- setup_cpp(k = 3, p0 = 0.2)
  res1 <- get_details(design = design1, n = 15, p1 = c(0.2, 0.2, 0.5),
    lambda = 0.99, tune_a = 2, tune_b = 2, iter = 5000)

  # Works without supplied p1
  expect_no_error(get_details(design = design, n = 15, p1 = NULL, lambda = 0.99,
    tune_a = 2, tune_b = 2, iter = 5000))

  # Compare with results from baskexact
  expect_true(all(abs(res1$Rejection_Probabilities -
      c(0.06643573, 0.06643573, 0.56254586)) < 0.01))
  expect_true(all(abs(res1$Mean - c(0.2529584 , 0.2529584 , 0.4173126)) < 0.01))
  expect_true(all(abs(res1$MSE -
      c(0.008506501, 0.008506501, 0.018349713)) < 0.01))

  # With 4 baskets
  set.seed(20240308)
  design2 <- setup_cpp(k = 4, p0 = 0.15)
  res2 <- get_details(design = design2, n = 15, p1 = c(0.15, 0.15, 0.15, 0.4),
    lambda = 0.99, tune_a = 1.5, tune_b = 1.5, iter = 5000)

  # Compare with results from baskexact
  expect_true(all(abs(res2$Rejection_Probabilities -
      c(0.05833261, 0.05833261, 0.05833261, 0.40165309)) < 0.015))
  expect_true(abs(res2$FWER - 0.1142539) < 0.01)
  expect_true(all(abs(res2$Mean -
      c(0.1938748, 0.1938748, 0.1938748, 0.2966433)) < 0.01))
  expect_true(all(abs(res2$MSE -
      c(0.005062454, 0.005062454, 0.005062454, 0.017710313)) < 0.01))
  expect_true(abs(res2$ECD - 3.226655) < 0.01)
  # If n is passed as a vector, it should have k entries
  expect_error(get_details.cpp(design = design, n = c(10,20), p1 = NULL,
                               lambda = 0.95, pmp0 = 1, data = NULL, iter = 110))
  expect_error(get_details.cpp(design = design, n = c(10,20,30,40), p1 = NULL,
                               lambda = 0.95, pmp0 = 1, data = NULL, iter = 110))
})

test_that("get_details works for cppglobal", {
  # With 3 baskets
  set.seed(20230512)
  design1 <- setup_cppglobal(k = 3, p0 = 0.15)
  res1 <- get_details(design = design1, n = 15, p1 = c(0.2, 0.4, 0.5),
    lambda = 0.98, tune_a = 1.5, tune_b = 1.5, epsilon = 2.5, iter = 5000)

  # Compare with results from baskexact
  expect_true(all(abs(res1$Rejection_Probabilities -
      c(0.3270933, 0.8168261, 0.9432173)) < 0.01))
  expect_true(all(abs(res1$Mean - c(0.2764610, 0.3967091, 0.4596072)) < 0.01))
  expect_true(all(abs(res1$MSE -
      c(0.013143002, 0.009512997, 0.013149404)) < 0.01))

  # With 4 baskets
  set.seed(20240308)
  design2 <- setup_cppglobal(k = 4, p0 = 0.15)
  res2 <- get_details(design = design2, n = 20, p1 = c(0.15, 0.25, 0.35, 0.45),
    lambda = 0.97, tune_a = 1.5, tune_b = 1, epsilon = 2, iter = 5000)

  # Compare with results from baskexact
  expect_true(all(abs(res2$Rejection_Probabilities -
      c(0.2270686, 0.5359827, 0.8157377, 0.9551109)) < 0.01))
  expect_true(all(abs(res2$Mean -
      c(0.2232156, 0.2794798, 0.3387829, 0.4002807)) < 0.01))
  expect_true(all(abs(res2$MSE -
      c(0.008987673, 0.005730799, 0.006345472, 0.010459698)) < 0.01))
  expect_true(abs(res2$ECD - 3.079763) < 0.01)

})

test_that("get_details works for mml", {
  # With 3 baskets
  set.seed(20240320)
  design1 <- setup_mml(k = 3, p0 = 0.2)
  res1 <- get_details(design = design1, n = 15, p1 = c(0.2, 0.4, 0.6),
    lambda = 0.95, iter = 5000)

  # Compare with results from baskexact
  expect_true(all(abs(res1$Rejection_Probabilities -
      c(0.2317285, 0.7058685, 0.9801984)) < 0.01))
  expect_true(all(abs(res1$Mean - c(0.2606807, 0.4056437, 0.5567046)) < 0.01))
  expect_true(abs(res1$ECD - 2.454338) < 0.01)

  # With 4 baskets
  set.seed(20240308)
  design2 <- setup_mml(k = 4, p0 = 0.15)
  res2 <- get_details(design = design2, n = 20, p1 = c(0.15, 0.15, 0.4, 0.4),
    lambda = 0.95, iter = 5000)

  # Compare with results from baskexact
  expect_true(all(abs(res2$Rejection_Probabilities -
      c(0.2614598, 0.2614598, 0.9254998, 0.9254998)) < 0.01))
  expect_true(abs(res2$FWER - 0.3914886) < 0.01)
  expect_true(all(abs(res2$Mean -
      c(0.1943315, 0.1943315, 0.3738412, 0.3738412)) < 0.01))
  expect_true(all(abs(res2$MSE -
      c(0.006931833, 0.006931833, 0.010724183, 0.010724183)) < 0.01))
  expect_true(abs(res2$ECD - 3.32808) < 0.01)
})


test_that("get_details works for cpplim", {
  # compare with CPP (w = 1, alpha_0 = 1, gamma = 0)
  design_cpplim <- setup_cpplim(k = 3, p0 = 0.2)
  design_cpp <- setup_cpp(k = 3, p0 = 0.2)

  n <- 20
  data <- matrix(data = 15, ncol = 3, nrow = 100)
  p1 <- c(0.2,0.5,0.5)
  lambda <- 0.987

  res_cpplim <- get_details.cpplim(design = design_cpplim, n = n, p1 = p1,
                                   lambda = lambda, tune_a = 1, tune_b = 1,
                                   iter = 100, data = data)
  res_cpp <- get_details.cpp(design = design_cpp, n = n, p1 = p1, lambda = lambda,
                             tune_a = 1, tune_b = 1, iter = 100, data = data)

  expect_equal(res_cpplim$Rejection_Probabilities, res_cpp$Rejection_Probabilities)
  expect_equal(res_cpplim$FWER, res_cpp$FWER)
  expect_equal(res_cpplim$Mean, res_cpp$Mean)
  expect_equal(res_cpplim$MSE, res_cpp$MSE)
  expect_equal(res_cpplim$Lower_CL, res_cpp$Lower_CL)
  expect_equal(res_cpplim$Upper_CL, res_cpp$Upper_CL)


  # Works without supplied p1
  expect_no_error(get_details(design = design_cpplim, n = 15, p1 = NULL, lambda = 0.99,
                              tune_a = 2, tune_b = 2, iter = 5000))


  # If n is passed as a vector, it should have k entries
  expect_error(get_details.cpplim(design = design_cpplim, n = c(10,20), p1 = NULL,
                               lambda = 0.95, pmp0 = 1, data = NULL, iter = 110))
  expect_error(get_details.cpplim(design = design_cpplim, n = c(10,20,30,40), p1 = NULL,
                               lambda = 0.95, pmp0 = 1, data = NULL, iter = 110))
})

test_that("get_details works for app", {

  # compare with CPP (w = 1, alpha_0 = 1, gamma = 0)
  design_app <- setup_app(k = 3, p0 = 0.2)
  design_cpp <- setup_cpp(k = 3, p0 = 0.2)

  n <- 20
  data <- matrix(data = 15, ncol = 3, nrow = 100)
  p1 <- c(0.2,0.5,0.5)
  lambda <- 0.987

  res_app <- get_details.app(design = design_app, n = n, p1 = p1, lambda = lambda,
                             iter = 100, data = data)
  res_cpp <- get_details.cpp(design = design_cpp, n = n, p1 = p1, lambda = lambda,
                             tune_a = 1, tune_b = 1, iter = 100, data = data)

  expect_equal(res_app$Rejection_Probabilities, res_cpp$Rejection_Probabilities)
  expect_equal(res_app$FWER, res_cpp$FWER)
  expect_equal(res_app$Mean, res_cpp$Mean)
  expect_equal(res_app$MSE, res_cpp$MSE)
  expect_equal(res_app$Lower_CL, res_cpp$Lower_CL)
  expect_equal(res_app$Upper_CL, res_cpp$Upper_CL)


  # Works without supplied p1
  expect_no_error(get_details(design = design_app, n = 15, p1 = NULL, lambda = 0.99,
                              tune_a = 2, tune_b = 2, iter = 5000))



  # If n is passed as a vector, it should have k entries
  expect_error(get_details.app(design = design_app, n = c(10,20), p1 = NULL,
                                  lambda = 0.95, pmp0 = 1, data = NULL, iter = 110))
  expect_error(get_details.app(design = design_app, n = c(10,20,30,40), p1 = NULL,
                                  lambda = 0.95, pmp0 = 1, data = NULL, iter = 110))
})
