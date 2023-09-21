#' Optimize a Basket Trial Design
#'
#' @template design
#' @template n
#' @template alpha
#' @template design_params
#' @param scenarios A matrix of scenarios.
#' @template prec_digits
#' @template iter
#' @param data A list of data matrices generated with \code{get_data}. The
#'   list elements have to correspond to the columsn of \code{scenarios}.
#' @template dotdotdot
#'
#' @return A matrix with the expected number of correct decisions.
#' @export
#'
#' @examples
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#' scenarios <- get_scenarios(design, p1 = 0.5)
#'
#' # Without simulated data
#' opt_design(design, n = 20, alpha = 0.05, design_params =
#'   list(epsilon = c(1, 2), tau = c(0, 0.5)), scenarios = scenarios,
#'   prec_digits = 3)
#'
#' # With simulated data
#' scenario_list <- as.list(data.frame(scenarios))
#' data_list <- lapply(scenario_list,
#'   function(x) get_data(k = 3, n = 20, p = x, iter = 1000))
#' opt_design(design, n = 20, alpha = 0.05, design_params =
#'   list(epsilon = c(1, 2), tau = c(0, 0.5)), scenarios = scenarios,
#'   prec_digits = 3, data = data_list)
opt_design <- function(design, n, alpha, design_params = list(), scenarios,
                       prec_digits, iter = 1000, data = NULL, ...) {
  check_data_list(data = data, scenarios = scenarios)
  check_scenarios(scenarios = scenarios, design = design)
  grid <- expand.grid(design_params)
  if (length(design_params) == 0) {
    lgrid <- 1
  } else {
    lgrid <- nrow(grid)
  }
  p <- progressr::progressor(steps = lgrid)

  ecd_res <- matrix(nrow = lgrid, ncol = ncol(scenarios))
  colnames(ecd_res) <- colnames(scenarios)
  lambdas <- numeric(lgrid)
  null_scen <- which(apply(scenarios, 2, function(x) all(x == design$p0)))

  for (i in 1:lgrid) {
    params_loop <- lapply(as.list(grid), function(x) x[i])
    l <- do.call(adjust_lambda, args = list(design = design, n = n,
      p1 = NULL, alpha = alpha, design_params = params_loop, iter = iter,
      prec_digits = prec_digits, data = data[[null_scen]], ...))
    lambdas[i] <- l$lambda

    for (j in 1:ncol(scenarios)) {
      ecd_res[i, j] <- do.call(ecd, args = c(design = list(design), n = n,
        p1 = list(scenarios[, j]), lambda = l$lambda, params_loop,
        iter = iter, data = list(data[[j]]), ...))
    }
    p()
  }

  if (ncol(grid) == 0) {
    ecd_res <- cbind("Lambda" = lambdas, ecd_res,
      "Mean_ECD" = rowMeans(ecd_res))
  } else {
    ecd_res <- cbind(grid, "Lambda" = lambdas, ecd_res,
      "Mean_ECD" = rowMeans(ecd_res))
    ecd_res <- ecd_res[order(ecd_res[, ncol(ecd_res)], decreasing = TRUE), ]
  }
  ecd_res
}

#' Create a Scenario Matrix
#'
#' Creates a default scenario matrix.
#'
#' @template design
#' @param p1 Probability under the alternative hypothesis.
#'
#' @details \code{get_scenarios} creates a default scenario matrix
#' that can be used for \code{\link{opt_design}}. The function creates
#' \code{k + 1} scenarios, from a global null to a global alternative scenario.
#'
#' @return A matrix with \code{k} rows and \code{k + 1} columns.
#' @export
#'
#' @examples
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#' get_scenarios(design = design, p1 = 0.5)
get_scenarios <- function(design, p1) {
  scen_mat <- matrix(nrow = design$k, ncol = design$k + 1)
  for (i in 0:design$k) {
    scen_mat[, (i + 1)] <- c(rep(design$p0, design$k - i),
      rep(p1, i))
  }
  colnames(scen_mat) <- paste(0:design$k, "Active")
  scen_mat
}

#' Optimize a Basket Trial Design
#'
#' Optimize the parameters of a basket trial design using a utility-based
#' approach with a simulation algorithm of your choice.
#'
#' @template design
#' @template utility
#' @template algorithm
#' @template detail_params
#' @template utility_params
#' @template algorithm_params
#' @template trace
#'
#' @inherit optimization_optim_sa return
#'
#' @export
#'
#' @examples
#' design <- setup_fujikawa(k = 3, p0 = 0.2, shape1 = 1, shape2 = 1)
#' opt_design_gen(design = design,
#'                utility = u_powfwer_discont,
#'                algorithm = optimization_optim_sa,
#'                detail_params = list(n = 20, p1 = c(0.5, 0.2, 0.2),
#'                                     logbase = exp(1), exact = TRUE),
#'                utility_params = list(alpha = 0.05),
#'                algorithm_params = list(start = c(lambda = 0.99,
#'                                                     epsilon = 2,
#'                                                     tau = 0.5),
#'                                        maximization = TRUE,
#'                                        lower = c(lambda = 0.001,
#'                                                     epsilon = 1,
#'                                                     tau = 0.001),
#'                                        upper = c(lambda = 0.999,
#'                                                     epsilon = 10,
#'                                                     tau = 0.999),
#'                                        control = list(t0 = 1000,
#'                                                       t_min = 0.1,
#'                                                       r = 0.6,
#'                                                       k = 1,
#'                                                       maxgood = 1)))
#' opt_design_gen(design = design,
#'                utility = u_powfwer_discont_bound,
#'                algorithm = stats_optim_sann,
#'                detail_params = list(n = 20, p1 = c(0.5, 0.2, 0.2),
#'                                     logbase = exp(1), exact = TRUE),
#'                utility_params = list(alpha = 0.05,
#'                                      lower = c(lambda = 0.001,
#'                                                epsilon = 1,
#'                                                tau = 0.001),
#'                                       upper = c(lambda = 0.999,
#'                                                 epsilon = 10,
#'                                                 tau = 0.999)),
#'                algorithm_params = list(start = c(lambda = 0.99,
#'                                                     epsilon = 2,
#'                                                     tau = 0.5),
#'                                        maximization = TRUE,
#'                                        control = list(maxit = 30000,
#'                                                       temp = 2000,
#'                                                       REPORT = 2)))
opt_design_gen <- function(design, utility, algorithm, detail_params,
                           utility_params, algorithm_params, trace = TRUE){
  x_names <- character()
  if(!is.null(algorithm_params$lower)){
    x_names <- names(algorithm_params$lower)
  } else if(!is.null(algorithm_params$start)){
    x_names <- names(algorithm_params$start)
  } else {
    stop("Cannot retrieve parameter vector names from algorithm_params. Please
         supply a 'lower' or a 'start'  argument in algorithm_params.")
  }
  u_fun <- function(x){
    x_named <- x
    names(x_named) <- x_names
    do.call(utility, c(design = list(design),
                                          x = list(x_named),
                                          detail_params = list(detail_params),
                                          utility_params))}
  res <- do.call(algorithm,
                  c(fun = u_fun,
                    trace = trace,
                    algorithm_params))
}
