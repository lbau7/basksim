# Beta Borrowing for Limited CPP Design
beta_borrow_cpplim <- function(design, n, r, weights, alpha_0 = alpha_0){

  shape_noprior <- matrix(c(r, n - r), nrow = 2, byrow = TRUE)
  weight_mat <- get_weight_mat_pp(design, n, r, weights)

  # Compute posterior shapes
  shape1post <- apply(alpha_0*weight_mat, 1, function(x) sum(shape_noprior[1, ] * x)) +
    design$shape1
  shape2post <- apply(alpha_0*weight_mat, 1, function(x) sum(shape_noprior[2, ] * x)) +
    design$shape2
  rbind(shape1post, shape2post)

}




# Analyzing Results for limited CPP Design
ana_cpplim <- function(design, n, r, lambda, weights, alpha_0){
  shape_post <- beta_borrow_cpplim(design = design, n = n, r = r,
                                   weights = weights, alpha_0 = alpha_0)
  post_prob <- post_beta(shape = shape_post, p0 = design$p0)
  ifelse(post_prob >= lambda, 1, 0)
}






