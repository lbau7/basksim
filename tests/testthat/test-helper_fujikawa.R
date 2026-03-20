test_that("beta_borrow_fujikawa works", {
  design <- setup_fujikawa(k = 4, p0 = 0.2)
  r <- c(9, 14, 1, 5)

  weight_mat <- get_weights_jsd(design, n = 20, epsilon = 1, tau = 0,
    logbase = 2)
  res1 <- beta_borrow_fujikawa(design = design, n = 20, r = r,
    weights = weight_mat)
  res2 <- val_borrow_fujikawa(design = design, n = 20, r = r, epsilon = 1,
    tau = 0, logbase = 2)
  res3 <- beta_borrow_fujikawa(design = design, n = 20, r = rev(r),
    weights = weight_mat)

  expect_equal(unname(res1), unname(res2))
  expect_equal(res1, res3[, 4:1])
})

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
