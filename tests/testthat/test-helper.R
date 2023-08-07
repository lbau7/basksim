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
