# Weight matrix with JSD weights

get_weights_jsd <- function(design, n, epsilon, tau, logbase, ...) {

  # the format of the weights depends on whether the sample sizes of the
  # individual baskets are all the same
  if(length(unique(n)) == 1 || length(n) == 1){

    shape1_post <- design$shape1 + c(0:n)
    shape2_post <- design$shape2 + c(n:0)
    n_sum <- n + 1

    p <- function(x) stats::dbeta(x, shape1_post[i], shape2_post[i])
    q <- function(x) stats::dbeta(x, shape1_post[j], shape2_post[j])
    m <- function(x) 0.5 * (p(x) + q(x))
    f <- function(x) p(x) * log(p(x) / m(x), base = logbase)
    g <- function(x) q(x) * log(q(x) / m(x), base = logbase)

    jsd_mat <- matrix(0, nrow = n_sum, ncol = n_sum)
    for (i in 1:n_sum) {
      for (j in i:n_sum) {
        if (i == j) {
          next
        } else {
          kl_f <- stats::integrate(f, 0, 1)$value
          kl_g <- stats::integrate(g, 0, 1)$value
          jsd_mat[i, j] <- 0.5 * kl_f + 0.5 * kl_g
        }
      }
    }
    jsd_mat <- jsd_mat + t(jsd_mat)
    weight_mat <- (1 - jsd_mat)^epsilon
    weight_mat[weight_mat <= tau] <- 0

    weight_mat

  }else{

    # find all possible combinations
    unique_combs <- unique(t(apply(arrangements::combinations(k = 2, v = n),1,sort)))

    weights_list <- list()

    for(l in 1:dim(unique_combs)[1]){
      dim_act <- unique_combs[l,]
      n1 <- dim_act[1]
      n2 <- dim_act[2]

      jsd_mat <- matrix(0, nrow = n1+1, ncol = n2+1)

      shape1_post1 <- design$shape1 + c(0:n1)
      shape1_post2 <- design$shape1 + c(0:n2)

      shape2_post1 <- design$shape2 + c(n1:0)
      shape2_post2 <- design$shape2 + c(n2:0)

      p <- function(x) stats::dbeta(x, shape1_post1[i], shape2_post1[i])
      q <- function(x) stats::dbeta(x, shape1_post2[j], shape2_post2[j])
      m <- function(x) 0.5 * (p(x) + q(x))
      f <- function(x) p(x) * log(p(x) / m(x), base = logbase)
      g <- function(x) q(x) * log(q(x) / m(x), base = logbase)

      for (i in 1:(n1+1) ) {
        for (j in 1:(n2+1)) {

          kl_f <- stats::integrate(f, 0, 1)$value
          kl_g <- stats::integrate(g, 0, 1)$value
          jsd_mat[i, j] <- 0.5 * kl_f + 0.5 * kl_g

        }
      }

      weight_mat <- (1 - jsd_mat)^epsilon
      weight_mat[weight_mat <= tau] <- 0

      weights_list[[l]] <- weight_mat

    }

    weights_list

  }




}



# Weight matrix with CPP weights
get_weights_cpp <- function(n, tune_a = 1, tune_b = 1, ...) {

  # The format of the weights depends on whether the sample sizes of the
  # individual baskets are all the same
  if(length(unique(n)) == 1 || length(n) == 1){

    n <- ifelse(length(n) == 1, n, n[1])

     n_sum <- n + 1
     weight_mat <- matrix(0, nrow = n_sum, ncol = n_sum)
     r1 <- r2 <- 0:n

     g <- function(s, a, b) {
       1 / (1 + exp(a + b * log(s)))
     }

     for (i in 1:n_sum) {
       for (j in i:n_sum) {
         if (i == j) {
           next
         } else {
           vec1 <- rep(0:1, c(n - r1[i], r1[i]))
           vec2 <- rep(0:1, c(n - r2[j], r2[j]))
           ks <- suppressWarnings(stats::ks.test(vec1, vec2)$statistic)
           s <- n^(1/4) * ks
           weight_mat[i, j] <- g(s = s, a = tune_a, b = tune_b)
         }
       }
     }
     weight_mat <- weight_mat + t(weight_mat)
     diag(weight_mat) <- 1
     weight_mat

  }else{

    # find all possible combinations
    unique_combs <- unique(t(apply(arrangements::combinations(k = 2, v = n),1,sort)))

    g <- function(s, a, b) {
      1 / (1 + exp(a + b * log(s)))
    }

    weights_list <- list()

    for(l in 1:dim(unique_combs)[1]){
      dim_act <- unique_combs[l,]
      n1 <- dim_act[1]
      n2 <- dim_act[2]

      weight_mat <- matrix(0, nrow = n1+1, ncol = n2+1)
      r1 <- 0:n1
      r2 <- 0:n2

      for(i in 1:length(r1)){
        for(j in 1:length(r2)){
          vec1 <- rep(0:1, c(n1 - r1[i], r1[i]))
          vec2 <- rep(0:1, c(n2 - r2[j], r2[j]))
          ks <- suppressWarnings(stats::ks.test(vec1, vec2)$statistic)
          s <- max(n1,n2)^(1/4) * ks
          weight_mat[i, j] <- g(s = s, a = tune_a, b = tune_b)
        }
      }
      weights_list[[l]] <- weight_mat
    }

    weights_list

  }

}



# Calculate Posterior Probabilites of a Beta Distribution
post_beta <- function(shape, p0) {
  stats::pbeta(p0, shape1 = shape[1, ], shape2 = shape[2, ],
    lower.tail = FALSE)
}

cfun1 <- function(x, y) {
  list(
    rbind(x[[1]], y[[1]]),
    rbind(x[[2]], y[[2]]),
    rbind(x[[3]], y[[3]]),
    rbind(x[[4]], y[[4]])
  )
}

cfun2 <- function(x, y) {
  list(
    rbind(x[[1]], y[[1]]),
    rbind(x[[2]], y[[2]])
  )
}

check_scenarios <- function(scenarios, design) {
  if (!(is.matrix(scenarios) | is.data.frame(scenarios))) {
    stop("scenarios is not a matrix or a data.frame")
  }
  if (sum(duplicated(t(scenarios))) > 0) {
    stop("not all scenarios are distinct")
  }
  if (sum(apply(scenarios, 2, function(x) all(x == design$p0))) == 0) {
    stop("no null scenario")
  }
  if (nrow(scenarios) != design$k) {
    stop("scenarios doesn't have k rows")
  }
  if (!all(scenarios > 0) | !all(scenarios < 1)) {
    stop("probabilities have to be in (0, 1)")
  }
}
