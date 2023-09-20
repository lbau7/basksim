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
}
