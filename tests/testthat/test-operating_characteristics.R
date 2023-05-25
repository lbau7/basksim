test_that("toer works", {
  set.seed(20230319)
  design_sim <- setup_fujikawa(k = 3, p0 = 0.2)
  res <- toer(design = design_sim, n = 15, lambda = 0.99, design_params =
      list(epsilon = 2, tau = 0), iter = 5000)

  # Compare with results from baskexact
  expect_true(abs(0.03845609 - res) < 0.01)
})

test_that("ecd works", {
  set.seed(20230512)
  design <- setup_cpp(k = 3, p0 = 0.2)
  res <- ecd(design = design, n = 15, p1 = c(0.2, 0.2, 0.5), lambda = 0.95,
    design_params = list(tune_a = 1, tune_b = 1), iter = 5000)

  # Compare with results from baskexact
  expect_true(abs(2.338286 - res) < 0.01)
})
