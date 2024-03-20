test_that("get_weights_cpp works", {
  wmat <- get_weights_cpp(n = 5, tune_a = 1, tune_b = 1)
  w1 <- 1 / (1 + exp(1 + 1 * log(5^(1/4) * abs(0/5 - 1/5))))
  w2 <- 1 / (1 + exp(1 + 1 * log(5^(1/4) * abs(0/5 - 2/5))))

  expect_equal(wmat[1, 2], w1)
  expect_equal(wmat[1, 3], w2)
})
