test_that("get_details works for bma", {
  design <- setup_bma(k = 3, p0 = 0.2)
  set.seed(20230222)
  res1 <- get_details(design = design, n = 20, p1 = c(0.2, 0.4, 0.4),
    lambda = 0.95, pmp0 = 1, iter = 100)

  res2 <- get_details(design = design, n = 20, p1 = c(0.3, 0.5, 0.5),
    lambda = 0.95, pmp0 = 1, iter = 100)

  res3 <- get_details(design = design, n = 20, p1 = c(0.5, 0.5, 0.5),
    lambda = 0.95, pmp0 = 1, iter = 100)

  # Rejection probabilities are higher when p is higher
  expect_true(all(res2$Rejection_Probabilities > res1$Rejection_Probabilities))

  # Posterior means are close to p
  expect_true(all(abs(res3$Mean - 0.5) < 0.02))
})

test_that("get_details works for ebcomb", {
  design <- setup_ebcomb(k = 3, p0 = 0.2)
  set.seed(20230222)
  res1 <- get_details(design = design, n = 20, p1 = c(0.2, 0.4, 0.4),
    lambda = 0.95, pmp0 = 1, iter = 100)

  res2 <- get_details(design = design, n = 20, p1 = c(0.3, 0.5, 0.5),
    lambda = 0.95, pmp0 = 1, iter = 100)

  res3 <- get_details(design = design, n = 20, p1 = c(0.5, 0.5, 0.5),
    lambda = 0.95, pmp0 = 1, iter = 100)

  # Rejection probabilities are higher when p is higher
  expect_true(all(res2$Rejection_Probabilities > res1$Rejection_Probabilities))

  # Posterior means are close to p
  expect_true(all(abs(res3$Mean - 0.5) < 0.02))
})

# test_that("get_details works for bhm", {
#   design <- setup_bhm(k = 3, p0 = 0.15, p_target = c(0.2, 0.2, 0.3, 0.4),
#     mu_mean = -1.7346, mu_sd = 100)
#
#   get_details(design = design, n = 20,
#     p1 = rep(0.15, 3), lambda = 0.9, tau_scale = 0.75, iter = 5000)
# })

test_that("get_details works for fujikawa", {
  set.seed(20230319)
  design_sim <- setup_fujikawa(k = 3, p0 = 0.2)
  design_exact <- baskexact::setupOneStageBasket(k = 3, shape1 = 1,
    theta0 = 0.2)

  res_sim <- get_details(design = design_sim, n = 15, p1 = c(0.2, 0.2, 0.5),
    lambda = 0.99, epsilon = 2, tau = 0, iter = 500)$Rejection_Probabilities
  res_exact <- baskexact::pow(design = design_exact, theta1 = c(0.2, 0.2, 0.5),
    n = 15, lambda = 0.99, epsilon = 2, tau = 0, logbase = exp(1),
    results = "group")$rejection_probabilities

  expect_true(all(abs(res_sim - res_exact) < 0.02))
})

# test_that("get_details works for cpp", {
#   set.seed(20230319)
#   design_sim <- setup_cpp(k = 3, p0 = 0.2)
#   design_exact <- baskexact::setupOneStageBasket(k = 3, theta0 = 0.2)
#
#   res_sim <- get_details(design = design_sim, n = 15, p1 = c(0.2, 0.2, 0.5),
#     lambda = 0.99, tune_a = 1, tune_b = 1, iter = 500)
#   # res_exact <-
# })
