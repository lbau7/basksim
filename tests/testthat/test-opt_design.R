test_that("opt_design works", {
  design1 <- setup_fujikawa(k = 3, p0 = 0.2)
  scenarios <- get_scenarios(design = design1, p1 = 0.5)

  set.seed(20230517)
  res1 <- opt_design(design = design1, n = 10, alpha = 0.05,
    design_params = list(epsilon = c(1, 2), tau = 0), scenarios = scenarios,
    prec_digits = 3)

  ecdres1 <- ecd(design = design1, n = 10, p1 = c(0.2, 0.2, 0.5),
    lambda = res1[1, 3], design_params = list(epsilon = 2, tau = 0))

  expect_equal(mean(as.numeric(res1[1, 4:7])), unname(res1[1, 8]))
  expect_true(abs(res1[1, 5] - ecdres1) < 0.1)

  design2 <- setup_ebcomb(k = 3, p0 = 0.2)
  res2 <- opt_design(design = design2, n = 10, alpha = 0.05,
    scenarios = scenarios, prec_digits = 3)

  ecdres2 <- ecd(design = design2, n = 10, p1 = c(0.2, 0.2, 0.5),
    lambda = res1[1, 3])

  expect_equal(mean(as.numeric(res2[1, 2:5])), unname(res2[1, 6]))
  expect_true(abs(res2[1, 3] - ecdres2) < 0.1)
})
