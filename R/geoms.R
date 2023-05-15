#' Plot a Bayesian basket trial's prior distribution
#'
#' @template design
#' @param repeat repeat the identical plot for each basket?
#' @template dotdotdot
#'
#' @return A ggplot object with a plot of the probability density function for
#'  each basket.
#' @export
#'
#' @examples
#' # Example for a basket trial with Fujikawa's Design
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#' geom_prior(design)
geom_prior <- function(design, ...) {
  UseMethod("geom_prior", design)
}

geom_prior.fujikawa <- function(design, repeat = TRUE){
  purrr::pmap(data.frame(basket = (1:design$k)),
             function(basket) {
            geom_function(data = data.frame(basket = basket),
                  fun = dbeta,
                  args = list(shape1 = design$shape1,
                              shape2 = design$shape2))
               }
            )
}

geom_posterior.fujikawa <- function(design, n, r, repeat = TRUE){

  purrr::pmap(cbind(t(beta_post(design, n, r)),
                    data.frame(basket = (1:design$k))),
              function(basket) {
                geom_function(data = data.frame(basket = basket),
                              fun = dbeta,
                              args = list(shape1 = design$shape1,
                                          shape2 = design$shape2))
              }
  )
}

geom_borrow.fujikawa <- function(design, n, r, lambda, epsilon, tau,
                                 repeat = TRUE){
  weights <- get_weights_jsd(design = design, n = n, epsilon = epsilon,
                                        tau = tau)
  purrr::pmap(cbind(t(beta_borrow_fujikawa(design, n, r, weights)),
                    data.frame(basket = (1:design$k))),
              function(basket) {
                geom_function(data = data.frame(basket = basket),
                              fun = dbeta,
                              args = list(shape1 = design$shape1,
                                          shape2 = design$shape2))
              }
  )
}
