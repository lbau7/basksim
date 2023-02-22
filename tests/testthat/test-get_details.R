test_that("get_details works for bma", {
  design <- setup_bma(k = 3, p0 = 0.2)
  set.seed(20230222)
  res1 <- get_details(design = design, n = 20, p1 = c(0.2, 0.4, 0.4),
    lambda = 0.95, pmp0 = 1, iter = 1000)

  res2 <- get_details(design = design, n = 20, p1 = c(0.3, 0.5, 0.5),
    lambda = 0.95, pmp0 = 1, iter = 100)

  res3 <- get_details(design = design, n = 20, p1 = c(0.5, 0.5, 0.5),
    lambda = 0.95, pmp0 = 1, iter = 100)

  # Rejection probabilities are higher when p is higher
  expect_true(all(res2$Rejection_Probabilities > res1$Rejection_Probabilities))
})
