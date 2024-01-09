test_that("diff_all works", {
  res1 <- diff_all(n = 20, r = c(0, 10, 20), epsilon = 1)
  res2 <- diff_all(n = 20, r = c(5, 5, 5), epsilon = 1)

  expect_equal(res1, 0)
  expect_equal(res2, 1)
})
