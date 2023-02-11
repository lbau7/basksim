# Beta Borrowing for Generalized CPP Design
beta_borrow_cppgen <- function(design, n, r, weights_pair, epsilon) {
  shape_noprior <- matrix(c(r, n - r), nrow = 2, byrow = TRUE)
  weight_all <- diff_all(r, epsilon = epsilon)

  # Compute pairwise weights and multiply by global weight
  all_combs <- arrangements::combinations(r, 2) + 1
  weights_vec <- weights_pair[all_combs] * weight_all

  # Create weight k x k weight matrix
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

# Global Weights for CPP Gen Design
diff_all <- function(r, epsilon) {
  rs <- sort(r)
  d <- diff(rs)
  (1 - sum(d) * 10^(-sum((d - 1 / length(d))^2)))^epsilon
}

# Analyzing Results for CPP Design
ana_cppgen <- function(design, n, r, lambda, weights_pair, epsilon) {
  shape_post <- beta_borrow_cppgen(design = design, n = n, r = r,
    weights_pair = weights_pair, epsilon = epsilon)
  post_prob <- post_beta(shape = shape_post, p0 = design$p0)
  ifelse(post_prob >= lambda, 1, 0)
}
