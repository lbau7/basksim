test_that("check_scenarios works", {
  design <- setup_fujikawa(k = 3, p0 = 0.2)
  scenarios <- get_scenarios(design = design, p1 = 0.5)

  expect_error(check_scenarios(scenarios = scenarios[, -1], design = design))
  expect_error(check_scenarios(scenarios = cbind(scenarios, scenarios[, 1]),
    design = design))
  expect_error(check_scenarios(scenarios = list(scenarios),
    design = design))
  expect_error(check_scenarios(scenarios = scenarios[-1, -4], design = design))
  expect_error(check_scenarios(scenarios = cbind(scenarios, c(0.2, 0.2, 1)),
    design = design))
})


test_that("check_get_weights_cpp", {

  expect_equal(get_weights_cpp(n = 5, tune_a = 1, tune_b = 1),
               get_weights_cpp(n = c(5,5,6,7), tune_a = 1, tune_b = 1)[[1]])

})



test_that("check_get_weights_jsd", {

  design <- setup_fujikawa(k = 3, p0 = 0.2)

  expect_equal(get_weights_jsd(design = design, n = 5, epsilon = 2, tau = 0,
                               logbase = 2),
               get_weights_jsd(design = design, n = c(5,5,6,7), epsilon = 2,
                               tau = 0, logbase = 2)[[1]])

})



