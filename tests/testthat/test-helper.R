test_that("check_scenarios works", {
  design <- setup_fujikawa(k = 3, p0 = 0.2)
  scenarios <- get_scenarios(design = design, p1 = 0.5)

  expect_error(check_scenarios(scenarios = scenarios[, -1], design = design))
  expect_error(check_scenarios(scenarios = cbind(scenarios, scenarios[, 1]),
    design = design))
  expect_error(check_scenarios(scenarios = list(scenarios),
    design = design))
  expect_error(check_scenarios(scenarios = scenarios[-1, -4], design = design))
  expect_error(check_scenarios(scenarios = cbind(scenarios, c(0.2, 0.2, 1)),
    design = design))
})


test_that("check_get_weights_cpp", {

  # # check unique combs
  # expect_equal(unique(t(apply(arrangements::combinations(k = 2, v = c(1,2,3)),1,sort))),
  #              matrix(data = c(1,2,1,3,2,3), ncol=2, byrow = TRUE))

  validate_get_weights_cpp_diffn <- function(n, tune_a = 1, tune_b = 1){

    if(length(n) == 1){
      n <- c(n,n)
    }

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

  expect_equal(list(get_weights_cpp(n = 20, tune_a = 1, tune_b = 1)),
               validate_get_weights_cpp_diffn(n = 20, tune_a = 1, tune_b = 1))

})



