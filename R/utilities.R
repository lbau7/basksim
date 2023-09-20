#' Utility function: Discontinuous power function with type-I error penalty
#'
#' @template design
#' @template x
#' @template detail_params
#'
#' @return a numerical, the parameter combination's utility
#' @export
#'
#' @examples
#' design <- setup_fujikawa(k = 3, p0 = 0.2, shape1 = 1, shape2 = 1)
#' u_powfwer_discont(design,
#'                   x = list(lambda = 0.99, epsilon = 2, tau = 0.5),
#'                   detail_params = list(n = 20, p1 = c(0.5, 0.2, 0.2),
#'                                        logbase = exp(1), exact = TRUE),
#'                   alpha = 0.05)
u_powfwer_discont <- function(design, x, detail_params, alpha){
  det <- do.call(get_details, c(design = list(design),
                                as.list(x),
                                detail_params))
  if(det$FWER >= alpha){
    return(-det$FWER)
  } else{
    return(det$EWP)
  }
}
