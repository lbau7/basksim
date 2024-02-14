test_that("alpha_0 works", {
  set.seed(20230319)
  design <- setup_app(k = 3, p0 = 0.2)

  expect_equal(get_alpha_0_app(design = design, n = c(5,10,15)),
               matrix(data = c(1,5/10, 5/15, 1, 1, 10/15, 1, 1, 1),
                      ncol = 3, byrow = TRUE))

  expect_equal(get_alpha_0_app(design = design, n = 10),
               matrix(data = c(rep(1,9)), ncol = 3, byrow = TRUE))


})
