test_that("get_alpha_0_app works", {
  set.seed(20230319)
  design <- setup_app(k = 3, p0 = 0.2)

  expect_equal(get_alpha_0_app(design = design, n = c(5,10,15)),
               matrix(data = c(1,5/10, 5/15, 1, 1, 10/15, 1, 1, 1),
                      ncol = 3, byrow = TRUE))

  expect_equal(get_alpha_0_app(design = design, n = 10),
               matrix(data = c(rep(1,9)), ncol = 3, byrow = TRUE))


})

test_that("get_gamma works", {

  validate_gamma <- function(n, r) {
    l1 <- function(x) stats::dbinom(x = r[1], size = n[1], prob = x)^
      min(1, n[2] / n[1])
    l2 <- function(x) stats::dbinom(x = r[2], size = n[2], prob = x)^
      min(1, n[1] / n[2])
    l1_int <- integrate(l1, 0, 1)$value
    l2_int <- integrate(l2, 0, 1)$value
    l_all <- function(x) (sqrt(l1(x) / l1_int) - sqrt(l2(x) / l2_int))^2
    l_all_int <- integrate(l_all, 0, 1)$value
    sqrt(1 / 2 * l_all_int)
  }


  expect_equal(get_gamma(n_gamma = c(10,15), r_gamma = c(8,10)),
               validate_gamma(n = c(10,15), r = c(8,10)))


})


test_that("beta_borrow_app works", {
  # compare with CPP (w = 1, alpha_0 = 1, gamma = 0)
  design_app <- setup_app(k = 3, p0 = 0.2)
  design_cpp <- setup_cpp(k = 3, p0 = 0.2)

  n <- 15
  r <- rep(10,3)

  weights <- get_weights_cpp(n = n, tune_a = 1, tune_b = 1)
  alpha_0 <- get_alpha_0_app(design = design_app, n = n)

  expect_equal(beta_borrow_app(design = design_app, n = n, r = r, alpha_0 = alpha_0),
               beta_borrow_pp(design = design_cpp, n = n, r = r, weights = weights))


})

test_that("ana_app works", {
  # compare with CPP (w = 1, alpha_0 = 1, gamma = 0)
  design_app <- setup_app(k = 3, p0 = 0.2)
  design_cpp <- setup_cpp(k = 3, p0 = 0.2)

  n <- 15
  r <- rep(10,3)
  lambda <- 0.987

  weights <- get_weights_cpp(n = n, tune_a = 1, tune_b = 1)
  alpha_0 <- get_alpha_0_app(design = design_app, n = n)

  expect_equal(ana_app(design = design_app, n = n, r = r, lambda = lambda,
                       alpha_0 = alpha_0),
               ana_pp(design = design_cpp, n = n, r = r, lambda = lambda,
                       weights = weights))


})
