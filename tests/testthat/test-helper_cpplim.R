test_that("beta_borrow_cpplim works", {
  # compare with CPP (alpha_0 = 1)
  design_cpplim <- setup_app(k = 3, p0 = 0.2)
  design_cpp <- setup_cpp(k = 3, p0 = 0.2)

  n <- 15
  r <- rep(10,3)

  weights <- get_weights_cpp(n = n, tune_a = 1, tune_b = 1)
  alpha_0 <- get_alpha_0_app(design = design_cpplim, n = n)

  expect_equal(beta_borrow_cpplim(design = design_cpplim, n = n, r = r,
                                  weights = weights, alpha_0 = alpha_0),
               beta_borrow_cpp(design = design_cpp, n = n, r = r,
                               weights = weights))


})
