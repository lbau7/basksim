test_that("opt_design works", {
  design1 <- setup_fujikawa(k = 3, p0 = 0.2)
  scenarios <- get_scenarios(design = design1, p1 = 0.5)


  set.seed(01022024)
  # Without simulated data
  res1 <- opt_design(design = design1, n = 10, alpha = 0.05,
    design_params = list(epsilon = c(1, 2), tau = 0, logbase = exp(1)),
    scenarios = scenarios, prec_digits = 3)

  ecdres1 <- ecd(design = design1, n = 10, p1 = c(0.2, 0.2, 0.5),
    lambda = res1[1, 4], design_params = list(epsilon = 2, tau = 0,
      logbase = exp(1)))

  expect_equal(mean(as.numeric(res1[1, 5:8])), unname(res1[1, 9]))
  expect_true(abs(res1[1, 6] - ecdres1) < 0.1)

  ## Optimize MML global
  design2 <- setup_mmlglobal(k = 3, p0 = 0.2)
  res2 <- opt_design(design = design2, n = 10, alpha = 0.05,
                     scenarios = scenarios, prec_digits = 3)

  ecdres2 <- ecd(design = design2, n = 10, p1 = c(0.2, 0.2, 0.5),
                 lambda = as.numeric(res2[1, 1]))

  expect_equal(mean(as.numeric(res2[1, 2:5])), unname(res2[1, 6]))
  expect_true(abs(res2[1, 3] - ecdres2) < 0.1)

  # Optimize APP
  design5 <- setup_app(k = 3, p0 = 0.2)
  res5 <- opt_design(design = design5, n = 10, alpha = 0.05,
                     scenarios = scenarios, prec_digits = 3)

  ecdres5 <- ecd(design = design5, n = 10, p1 = c(0.2, 0.2, 0.5),
                 lambda = as.numeric(res5[1, 1]))

  expect_equal(mean(as.numeric(res5[1, 2:5])), unname(res5[1, 6]))
  expect_true(abs(res5[1, 3] - ecdres5) < 0.1)


  # With simulated data
  set.seed(123456)
  scenario_list <- as.list(data.frame(scenarios))
  data_list <- lapply(scenario_list,
    function(x) get_data(k = 3, n = 15, p = x, iter = 100))
  res3 <- opt_design(design = design1, n = 15, alpha = 0.05,
    design_params = list(epsilon = c(1, 2), tau = 0), scenarios = scenarios,
    prec_digits = 3, data = data_list, iter = 100)
  res4 <- opt_design(design = design1, n = 15, alpha = 0.05,
    design_params = list(epsilon = c(1, 2), tau = 0), scenarios = scenarios,
    prec_digits = 3, data = data_list, iter = 100)

  # Check if results are equal when opt_design is called two times with
  # the same simulate data
  expect_true(all(res3 == res4))

  lambdares <- adjust_lambda(design = design1, n = 15, p1 = NULL, alpha = 0.05,
    design_params = list(epsilon = 2, tau = 0), iter = 100, prec_digits = 3,
    data = data_list[[1]])
  ecdres <- ecd(design = design1, n = 15, p1 = 0.2, lambda = lambdares$lambda,
    design_params = list(epsilon = 2, tau = 0), iter = 100,
    data = data_list[[1]])

  expect_equal(lambdares$lambda, res3$Lambda[1])
  expect_equal(res3[1, 4], ecdres)
})



test_that("CPP and LCPP work",{
  design_cpp <- setup_cpp(k = 3, p0 = 0.15, shape1 = 1, shape2 = 1)
  design_cpplim <- setup_cpplim(k = 3, p0 = 0.15, shape1 = 1, shape2 = 1)

  n <- c(10,20,30)
  p <- c(rep(0.35,3))

  set.seed(12042024)
  data <- get_data(k = 3, n = n, p = p, iter = 100)

  res_cpp <- get_details(design = design_cpp, n = n, p1 = p, lambda = 0.95,
                         tune_a = 2, tune_b = 2, data = data, iter = 100)
  res_cpplim <- get_details(design = design_cpplim, n = n, p1 = p, lambda = 0.95,
                            tune_a = 2, tune_b = 2, data = data, iter = 100)

  # Since basket 3 has the largest sample size, alpha_o = 1 and both models should
  # have the same power in the largest basket.
  expect_equal(res_cpp$Rejection_Probabilities[3], res_cpplim$Rejection_Probabilities[3])

})
