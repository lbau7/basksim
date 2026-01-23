# Quantity of information parameter
get_alpha_0_app <- function(design, n){

  k <- design$k
  if(length(n) == 1) n <- c(rep(n,k))
  alpha_0 <- matrix(nrow = design$k, ncol = design$k)

  # rows define the current basket (columns the compared ones)
  for(i in 1:k){
    n_cur <- n[i]

    for(j in 1:k){
      n_comp <- n[j]
      alpha_0[i,j] <- ifelse(n_cur/n_comp < 1, n_cur/n_comp, 1)
    }
  }
  alpha_0
}


# Commensurability parameter
get_gamma <- function(n_gamma = n_gamma, r_gamma = r_gamma){

  n_cur <- n_gamma[1]
  n_comp <- n_gamma[2]

  l_cur <- function(x) stats::dbinom(r_gamma[1], n_gamma[1], x)
  l_comp <- function(x) stats::dbinom(r_gamma[2], n_gamma[2], x)

  l_min_cur <- function(x) l_cur(x)^min(1,(n_comp/n_cur))
  l_min_comp <- function(x) l_comp(x)^min(1,(n_cur/n_comp))

  r_cur <- function(x) sqrt(l_min_cur(x)/ stats::integrate(l_min_cur, 0, 1)$value)
  r_comp <- function(x) sqrt(l_min_comp(x)/ stats::integrate(l_min_comp, 0, 1)$value)

  diff_sq <- function(x) (r_cur(x) - r_comp(x))^2

  d_squared <- 0.5*stats::integrate(diff_sq, 0, 1)$value

  sqrt(d_squared)

}




# Beta borrowing for APP Design
beta_borrow_app <- function(design = design, n = n, r = data[i, ],
                alpha_0 = alpha_0){

  k <- design$k
  if(length(n) == 1) n <- c(rep(n,k))

  shape_noprior <- matrix(c(r, n - r), nrow = 2, byrow = TRUE)

  gamma <- matrix(nrow = design$k, ncol = design$k)

  for(i in 1:k){
    for(j in 1:k){
      gamma[i,j] <- get_gamma(n_gamma = c(n[i], n[j]), r_gamma = c(r[i], r[j]))
    }
  }

  alpha <- alpha_0*(1-gamma)

  # Compute posterior shapes
  shape1post <- apply(alpha, 1, function(x) sum(shape_noprior[1, ] * x)) +
    design$shape1
  shape2post <- apply(alpha, 1, function(x) sum(shape_noprior[2, ] * x)) +
    design$shape2
  rbind(shape1post, shape2post)

}




# Analyzing Results for CPP Design
ana_app <- function(design, n, r, lambda, alpha_0) {
  shape_post <- beta_borrow_app(design = design, n = n, r = r,
                                alpha_0 = alpha_0)
  post_prob <- post_beta(shape = shape_post, p0 = design$p0)
  ifelse(post_prob >= lambda, 1, 0)
}

