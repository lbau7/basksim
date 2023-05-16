test_that("jsd_gen works", {
  shape1 <- matrix(c(1, 1, 1, 10, 10, 10), ncol = 3, byrow = TRUE)
  jsdgen1 <- jsd_gen(shape1, 1)

  # (1 - JSD) is equal to 1 when all distributions are equal
  expect_equal(jsdgen1, 1)

  shape2 <- matrix(c(1 + 4, 1 + 6, 1 + 2, 1 + 8), ncol = 2)
  jsdgen2 <- jsd_gen(shape2, 2)

  # Compare with results from baskexact
  expect_equal(jsdgen2, 0.560588865)

  shape3 <- matrix(c(1, 100, 100, 1), ncol = 2, byrow = TRUE)
  jsdgen3 <- jsd_gen(shape3, 1)

  # (1 - JSD) is 0 when distributions are very different
  expect_equal(jsdgen3, 0)
})
