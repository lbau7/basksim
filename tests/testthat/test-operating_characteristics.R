test_that("toer works", {
  set.seed(20230319)
  design_sim <- setup_fujikawa(k = 3, p0 = 0.2)
  design_exact <- baskexact::setupOneStageBasket(k = 3, theta0 = 0.2)

  toer_sim <- toer(design = design_sim, n = 15, lambda = 0.99, design_params =
      list(epsilon = 2, tau = 0))
  toer_exact <- baskexact::toer(design = design_exact, n = 15, lambda = 0.99,
    epsilon = 2, tau = 0, logbase = exp(1))

  expect_true(abs(toer_exact- toer_sim) < 0.01)
})

# test_that("ecd works", {
#   set.seed(20230319)
#   design_sim <- setup_fujikawa(k = 3, p0 = 0.2)
#   design_exact <- baskexact::setupOneStageBasket(k = 3, theta0 = 0.2)
#
#   ecd_sim <- ecd(design = design_sim, n = 15, lambda = 0.99, design_params =
#       list(epsilon = 2, tau = 0))
#   ecd_exact <-
# })
