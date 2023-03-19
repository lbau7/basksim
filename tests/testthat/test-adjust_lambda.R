test_that("adjust_lambda works", {
  set.seed(20230319)
  design <- setup_cpp(k = 3, p0 = 0.2)
  res <- adjust_lambda(design = design, n = 15, alpha = 0.05,
    design_params = list(tune_a = 1, tune_b = 1), iter = 1000)
  toer <- toer(design, n = 15, lambda = res$lambda, design_params =
      list(tune_a = 1, tune_b = 1), iter = 1000)

  expect_true(abs(res$toer - 0.05) < 0.02)
  expect_true(abs(res$toer - toer) < 0.02)
})
