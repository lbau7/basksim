test_that("get_weight_mat_pp works", {
  design <- setup_cpp(k = 4, p0 = 0.2)

  n_int <- 5
  n_vec <- c(5,5,6,7)
  r <- c(4,5,5,3)

  weights_int <- get_weights_cpp(n = n_int, tune_a = 1, tune_b = 1)
  weights_vec <- get_weights_cpp(n = n_vec, tune_a = 1, tune_b = 1)

  expect_equal(get_weight_mat_pp(design = design, n = n_int, r = r, weights = weights_int)[1:2,1:2],
               get_weight_mat_pp(design = design, n = n_vec, r = r, weights = weights_vec)[1:2,1:2])

})


test_that("beta_borrow_pp works", {

  design <- setup_cpp(k = 3, p0 = 0.2)
  weights <- get_weights_cpp(n = c(4,6,8), tune_a = 1, tune_b = 1)

  expect_equal(rbind(shape1post = c(5.284732, 9.560643, 9.728394),
                     shape2post = c(5.826573, 6.601986, 6.78523)),
               beta_borrow_pp(design = design, n = c(4,6,8), r = c(1,4,5), weights = weights),
               tolerance = 1e-6)

})

test_that("beta_borrow_pp works", {
  design <- setup_cpp(k = 4, p0 = 0.2)
  n <- 20
  r <- c(10, 12, 14, 16)

  weight_mat <- get_weights_cpp(n = 20, a = 1, b = 1)
  res1 <- beta_borrow_pp(design = design, n = 20, r = r, weights = weight_mat)
  res2 <- val_borrow_cpp(design = design, n = 20, r = r, a = 1, b = 1)
  res3 <- beta_borrow_pp(design = design, n = 20, r = rev(r),
    weights = weight_mat)

  expect_equal(unname(res1), unname(res2))
  expect_equal(res1, res3[, 4:1])
})
