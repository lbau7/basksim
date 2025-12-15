# Beta Posterior Distribution
beta_post <- function(design, n, r) {
  shape <- matrix(c(design$shape1 + r, design$shape2 + n - r), nrow = 2,
                  byrow = TRUE)
  rownames(shape) <- c("shape1post", "shape2post")
  return(shape)
}

# Beta Borrowing for Fujikawa's Design
beta_borrow_fujikawa <- function(design, n, r, weights) {
  if(length(unique(n)) == 1 || length(n) == 1){

    shape <- matrix(c(design$shape1 + r, design$shape2 + n - r), nrow = 2,
                    byrow = TRUE)
    all_combs <- arrangements::combinations(r, 2) + 1
    weights_vec <- weights[all_combs]
    weight_mat <- matrix(0, nrow = design$k, ncol = design$k)
    weight_mat[lower.tri(weight_mat)] <- weights_vec
    weight_mat <- weight_mat + t(weight_mat)
    diag(weight_mat) <- 1

  } else{

    shape <- matrix(c(design$shape1 + r, design$shape2 + n - r), nrow = 2,
                    byrow = TRUE)

    # find all possible combinations
    unique_combs <- unique(t(apply(arrangements::combinations(k = 2, v = n),1,sort)))

    k <- design$k

    weight_mat <- matrix(data = NA, nrow = k, ncol = k)

    # rows define the current basket (columns the compared ones)
    for(i in 1:k){
      n_cur <- n[i]

      for(j in 1:k){
        if(i == j){
          weight_mat[i,j] <- 1
          next
        }else{
          n_comp <- n[j]

          # The same basket is used for both combinations
          index <- which((unique_combs[,1] == n_cur & unique_combs[,2] == n_comp) | (unique_combs[,2] == n_cur & unique_combs[,1] == n_comp))
          matching_weights <- weights[[index]]

          # Ensure that the rows indicate the current basket
          if(nrow(matching_weights) != n_cur +1){
            matching_weights <- t(matching_weights)
          }
        }

        weight_mat[i,j] <- matching_weights[r[i]+1, r[j]+1]

      }
    }
  }

  shape1post <- apply(weight_mat, 1, function(x) sum(shape[1, ] * x))
  shape2post <- apply(weight_mat, 1, function(x) sum(shape[2, ] * x))
  rbind(shape1post, shape2post)

}



# Analyzing Results for Fujikawa's Design
ana_fujikawa <- function(design, n, r, lambda, weights) {
  shape_post <- beta_borrow_fujikawa(design = design, n = n, r = r,
    weights = weights)
  post_prob <- post_beta(shape = shape_post, p0 = design$p0)
  ifelse(post_prob >= lambda, 1, 0)
}
