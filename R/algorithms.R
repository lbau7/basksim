#' Simulated annealing from the `optimization` package
#'
#' This function is wrapper of `optimization::optim_sa()`.
#'
#' @template fun
#' @template trace
#' @template start
#' @param maximization logical, should the function be maximized
#' @template lower
#' @template upper
#' @param control a list of further arguments to be passed to `optim_sa()`
#'
#' @return a list consisting of the optimization result vector, the optimal
#'  utility value, and the trace of the optimization algorithm
#' @export
#'
#' @examples
optimization_optim_sa <- function(fun, trace = FALSE,
                                  start, maximization = FALSE,
                                  lower, upper,
                                  control = list()){
  osa <- optimization::optim_sa(fun = fun,
                                start = start,
                                maximization = maximization,
                                trace = trace,
                                lower = lower,
                                upper = upper,
                                control = control
                                )
  r <- control$r
  len_trace <- (ceiling(log(control$t_min/control$t0, base = r)) + 1)
  res <- rbind(
    c(n_outer = 0,
      loss = fun(start),
      start,
      n_inner = NA,
      temperature = NA,
      goodcounter = NA,
      NA, NA, NA
    ),
    osa$trace,
    c(n_outer = len_trace,
      loss = osa$function_value,
      osa$par,
      n_inner = NA,
      temperature = osa$trace[len_trace - 1, "temperature"] *  r,
      goodcounter = NA,
      osa$control$rf
    )
  )
  x_opt <- osa$par
  names(x_opt) <- names(start)
  return(list(x_opt = x_opt,
              u_opt = osa$function_value,
              trace = res))
}
#' Simulated annealing from the `optim` function of the `stats` library
#'
#' This function is wrapper of `stats::optim(method = "SANN")`.
#'
#' @template fun
#' @param trace doesn't do anything
#' @template start
#' @param maximization logical, should the function be maximized
#' @param control a list of further arguments to be passed to `optim()`
#'
#' @return a list consisting of the optimization result vector, the optimal
#'  utility value, and the trace of the optimization algorithm
#' @export
#'
#' @examples
stats_optim_sann <- function(fun, trace = FALSE,
                             start, maximization = FALSE,
                             control){
  if(maximization){
    control = c(control, fnscale = -1)
  }
  osa <- optim(par = start, fn = fun, method = "SANN",
               control = control)
}
