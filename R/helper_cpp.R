# Beta Borrowing for CPP Design
beta_borrow_cpp <- function(design, n, r, weights) {
  shape_noprior <- matrix(c(r, n - r), nrow = 2, byrow = TRUE)
  all_combs <- arrangements::combinations(r, 2) + 1
  weights_vec <- weights[all_combs]
  weight_mat <- matrix(nrow = design$k, ncol = design$k)
  weight_mat[lower.tri(weight_mat)] <- weight_mat[upper.tri(weight_mat)] <-
    weights_vec
  diag(weight_mat) <- 1

  # Compute posterior shapes
  shape1post <- apply(weight_mat, 1, function(x) sum(shape_noprior[1, ] * x)) +
    design$shape1
  shape2post <- apply(weight_mat, 1, function(x) sum(shape_noprior[2, ] * x)) +
    design$shape2
  rbind(shape1post, shape2post)
}

# Analyzing Results for CPP Design
ana_cpp <- function(design, n, r, lambda, weights) {
  shape_post <- beta_borrow_cpp(design = design, n = n, r = r,
    weights = weights)
  post_prob <- post_beta(shape = shape_post, p0 = design$p0)
  ifelse(post_prob >= lambda, 1, 0)
}
