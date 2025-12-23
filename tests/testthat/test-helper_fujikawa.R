test_that("get_weight_mat_jsd works", {
  design <- setup_fujikawa(k = 4, p0 = 0.2)

  n_int <- 5
  n_vec <- c(5,5,6,7)
  r <- c(4,5,5,3)

  weights_int <- get_weights_jsd(design = design, n = n_int, epsilon = 2,
                                 tau = 0, logbase = 2)
  weights_vec <- get_weights_jsd(design = design, n = n_vec, epsilon = 2,
                                 tau = 0, logbase = 2)

  expect_equal(get_weight_mat_jsd(design = design, n = n_int, r = r, weights = weights_int)[1:2,1:2],
               get_weight_mat_jsd(design = design, n = n_vec, r = r, weights = weights_vec)[1:2,1:2])


})
