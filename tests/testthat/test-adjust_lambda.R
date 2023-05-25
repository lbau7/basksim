test_that("adjust_lambda works", {
  design <- setup_cpp(k = 3, p0 = 0.2)

  set.seed(20230319)
  res1 <- adjust_lambda(design = design, n = 15, alpha = 0.05,
    design_params = list(tune_a = 1, tune_b = 1), iter = 1000)
  toer1 <- toer(design, n = 15, lambda = res1$lambda, design_params =
      list(tune_a = 1, tune_b = 1), iter = 1000)

  expect_lte(res1$toer, 0.05)
  expect_true(abs(res1$toer - toer1) < 0.02)

  set.seed(123)
  res2 <- adjust_lambda(design = design, n = 12, alpha = 0.05,
    design_params = list(tune_a = 2, tune_b = 2), iter = 1000)
  toer2 <- toer(design, n = 12, lambda = res2$lambda, design_params =
      list(tune_a = 2, tune_b = 2), iter = 1000)

  expect_lte(res2$toer, 0.05)
  expect_true(abs(res2$toer - toer2) < 0.02)

  set.seed(456)
  res3 <- adjust_lambda(design = design, n = 13, alpha = 0.05,
    design_params = list(tune_a = 4, tune_b = 1.5), iter = 1000)
  toer3 <- toer(design, n = 13, lambda = res3$lambda, design_params =
      list(tune_a = 4, tune_b = 1.5), iter = 1000)

  expect_lte(res3$toer, 0.05)
  expect_true(abs(res3$toer - toer3) < 0.02)
})
