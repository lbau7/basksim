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
