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

  # check unique combs
  expect_equal(unique(t(apply(arrangements::combinations(k = 2, v = c(1,2,3)),1,sort))),
               matrix(data = c(1,2,1,3,2,3), ncol=2, byrow = TRUE))


})
