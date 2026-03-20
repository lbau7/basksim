test_that("get_evaluation works for bhm", {
  design <- setup_bhm(k = 3, p0 = 0.2, p_target = 0.5)

  # Works with equal sample sizes
  expect_no_error(get_evaluation(design = design, n = c(20, 20, 20), r = c(10, 15, 5),
                                 lambda = 0.95, tau_scale = 1, iter = 100))

  # Works with unequal sample sizes
  expect_no_error(get_evaluation(design = design, n = c(15, 20, 25), r = c(10, 15, 17),
                                 lambda = 0.95, tau_scale = 1, iter = 100))

  # If n is passed as a vector, it should have k entries
  expect_error(get_evaluation(design = design, n = c(15, 20), r = c(10, 15, 17),
                              lambda = 0.95, tau_scale = 1, iter = 100))

  expect_error(get_evaluation(design = design, n = c(15, 20, 25, 30), r = c(10, 15, 17),
                              lambda = 0.95, tau_scale = 1, iter = 100))

})



test_that("get_evaluation works for exnex", {
  design <- setup_exnex(k = 3, p0 = 0.2)

  # Works with equal sample sizes
  expect_no_error(get_evaluation(design = design, n = c(20, 20, 20), r = c(10, 15, 5),
                                 lambda = 0.95, tau_scale = 1, w = 0.5, iter = 100))

  # Works with unequal sample sizes
  expect_no_error(get_evaluation(design = design, n = c(15, 20, 25), r = c(10, 15, 17),
                                 lambda = 0.95, tau_scale = 1, w = 0.5, iter = 100))

  # If n is passed as a vector, it should have k entries
  expect_error(get_evaluation(design = design, n = c(15, 20), r = c(10, 15, 17),
                              lambda = 0.95, tau_scale = 1, w = 0.5, iter = 100))

  expect_error(get_evaluation(design = design, n = c(15, 20, 25, 30), r = c(10, 15, 17),
                              lambda = 0.95, tau_scale = 1, w = 0.5, iter = 100))

})



test_that("get_evaluation works for fujikawa", {
  design <- setup_fujikawa(k = 3, p0 = 0.2)

  # Works with equal sample sizes
  expect_no_error(get_evaluation(design = design, n = 20, r = c(10, 15, 5), lambda = 0.95,
                                 epsilon = 2, tau = 0, iter = 100))

  # Works with unequal sample sizes
  expect_no_error(get_evaluation(design = design, n = c(15, 20, 25), r = c(10, 15, 17),
                                 lambda = 0.95, epsilon = 2, tau = 0, iter = 100))

  # If n is passed as a vector, it should have k entries
  expect_error(get_evaluation(design = design, n = c(15, 20), r = c(10, 15, 17),
                              lambda = 0.95, pmp0 = 1, iter = 100))

  expect_error(get_evaluation(design = design, n = c(15, 20, 25, 30), r = c(10, 15, 17),
                              lambda = 0.95, pmp0 = 1, iter = 100))

})



test_that("get_evaluation works for cpp", {
  design <- setup_cpp(k = 3, p0 = 0.2)

  # Works with equal sample sizes
  expect_no_error(get_evaluation(design = design, n = 20, r = c(10, 15, 5),
                                   lambda = 0.95, tune_a = 1, tune_b = 1, iter = 100))

  # Works with unequal sample sizes
  expect_no_error(get_evaluation(design = design, n = c(15, 20, 25), r = c(10, 15, 17),
                                 lambda = 0.95, tune_a = 1, tune_b = 1, iter = 100))

  # If n is passed as a vector, it should have k entries
  expect_error(get_evaluation(design = design, n = c(15, 20), r = c(10, 15, 17),
                              lambda = 0.95, tune_a = 1, tune_b = 1, iter = 100))

  expect_error(get_evaluation(design = design, n = c(15, 20, 25, 30), r = c(10, 15, 17),
                              lambda = 0.95, tune_a = 1, tune_b = 1, iter = 100))

})



test_that("get_evaluation works for cpplim", {
  design <- setup_cpplim(k = 3, p0 = 0.2)

  # Works with equal sample sizes
  expect_no_error(get_evaluation(design = design, n = 20, r = c(10, 15, 5),
                                 lambda = 0.95, tune_a = 1, tune_b = 1, iter = 100))

  # Works with unequal sample sizes
  expect_no_error(get_evaluation(design = design, n = c(15, 20, 25), r = c(10, 15, 17),
                                 lambda = 0.95, tune_a = 1, tune_b = 1, iter = 100))

  # If n is passed as a vector, it should have k entries
  expect_error(get_evaluation(design = design, n = c(15, 20), r = c(10, 15, 17),
                              lambda = 0.95, tune_a = 1, tune_b = 1, iter = 100))

  expect_error(get_evaluation(design = design, n = c(15, 20, 25, 30), r = c(10, 15, 17),
                              lambda = 0.95, tune_a = 1, tune_b = 1, iter = 100))

})



test_that("get_evaluation works for app", {
  design <- setup_app(k = 3, p0 = 0.2)

  # Works with equal sample sizes
  expect_no_error(get_evaluation(design = design, n = 20, r = c(10, 15, 5),
                                 lambda = 0.95, iter = 100))

  # Works with unequal sample sizes
  expect_no_error(get_evaluation(design = design, n = c(15, 20, 25), r = c(10, 15, 17),
                                 lambda = 0.95, iter = 100))

  # If n is passed as a vector, it should have k entries
  expect_error(get_evaluation(design = design, n = c(15, 20), r = c(10, 15, 17),
                              lambda = 0.95, iter = 100))

  expect_error(get_evaluation(design = design, n = c(15, 20, 25, 30), r = c(10, 15, 17),
                              lambda = 0.95, iter = 100))

})
