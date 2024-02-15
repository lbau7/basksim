test_that("get_weight_mat_cpp works", {
  design <- setup_cpp(k = 4, p0 = 0.2)

  n_int <- 5
  n_vec <- c(5,5,6,7)
  r <- c(4,5,5,3)

  weights_int <- get_weights_cpp(n = n_int, tune_a = 1, tune_b = 1)
  weights_vec <- get_weights_cpp(n = n_vec, tune_a = 1, tune_b = 1)

  expect_equal(get_weight_mat_cpp(design = design, n = n_int, r = r, weights = weights_int)[1:2,1:2],
               get_weight_mat_cpp(design = design, n = n_vec, r = r, weights = weights_vec)[1:2,1:2])

})
