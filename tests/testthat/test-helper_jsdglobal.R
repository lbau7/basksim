test_that("jsd_global works", {
  shape1 <- matrix(c(1, 1, 1, 10, 10, 10), ncol = 3, byrow = TRUE)
  jsdgen1 <- jsd_global(shape1, 1)

  # (1 - JSD) is equal to 1 when all distributions are equal
  expect_equal(jsdgen1, 1)

  shape2 <- matrix(c(1 + 4, 1 + 6, 1 + 2, 1 + 8), ncol = 2)
  jsdgen2 <- jsd_global(shape2, 2)

  # Compare with results from baskexact
  expect_equal(jsdgen2, 0.560588865)

  shape3 <- matrix(c(1, 100, 100, 1), ncol = 2, byrow = TRUE)
  jsdgen3 <- jsd_global(shape3, 1)

  # (1 - JSD) is 0 when distributions are very different
  expect_equal(jsdgen3, 0)
})

test_that("beta_borrow_jsdglobal works", {
  design <- setup_fujikawa(k = 3, p0 = 0.2)

  weights1 <- get_weights_jsd(design, n = 20, epsilon = 2, tau = 1,
    logbase = 2)
  res1 <- beta_borrow_jsdglobal(design, n = 20, r = c(1, 3, 5),
    weights_pair = weights1, eps_all = 0)
  res2 <- beta_borrow_fujikawa(design, n = 20, r = c(1, 3, 5),
    weights = weights1)

  # Results are identical for fujikawa and jsd global when no information
  # is shared
  expect_equal(res1, res2)

  weights2 <- get_weights_jsd(design, n = 20, epsilon = 2, tau = 0,
    logbase = 2)
  res3 <- beta_borrow_jsdglobal(design, n = 20, r = c(3, 3, 3),
    weights_pair = weights2, eps_all = 0)
  res4 <- beta_borrow_fujikawa(design, n = 20, r = c(3, 3, 3),
    weights = weights2)

  # Results differ only by the amount of prior information that is not shared
  expect_true(all(res3 + 2 == res4))
})
