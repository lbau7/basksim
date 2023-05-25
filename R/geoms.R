#' Plot a Bayesian basket trial's prior distribution
#'
#' @template design
#'
#' @return A list of ggplot layers of type `geom_function`.
#' @inherit geom_prior.fujikawa examples
#'
#' @export
geom_prior <- function(design, ...) {
  UseMethod("geom_prior")
}
#' Plot a Fujikawa basket trial's prior distribution
#'
#' @template dotdotdot_geom
#' @template design
#' @inherit geom_prior return
#'
#' @export
#'
#' @examples
#' # Example for a basket trial with Fujikawa's Design
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#' # One facet per basket
#' library(ggplot2)
#' ggplot() +
#'     geom_prior(design) +
#'     facet_wrap(vars(basket))
#' # Colour different baskets
#' ggplot() +
#'     geom_prior(aes(colour = basket), design)
geom_prior.fujikawa <- function(design, ...) {
  purrr::pmap(data.frame(basket = (1:design$k)),
              function(basket) {
                ggplot2::geom_function(
                  ...,
                  data = data.frame(basket = basket),
                  fun = dbeta,
                  args = list(shape1 = design$shape1,
                              shape2 = design$shape2)
                )
              })
}
#' Plot a Bayesian basket trial's posterior distribution
#'
#' @template design
#'
#' @return A list of ggplot layers of type `geom_function`.
#' @inherit geom_posterior.fujikawa examples
#'
#' @export
geom_posterior <- function(design, ...) {
  UseMethod("geom_posterior")
}
#' Plot a Fujikawa basket trial's posterior distribution
#'
#' @template dotdotdot_geom
#' @template design
#' @template n
#' @template r
#'
#' @inherit geom_posterior return
#'
#' @export
#'
#' @examples
#' # Example for a basket trial with Fujikawa's Design
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#' n <- 20
#' r <- c(4, 5, 2)
#' # One facet per basket
#' library(ggplot2)
#' ggplot() +
#'     geom_posterior(design, n, r) +
#'     facet_wrap(vars(basket))
#' # Colour different baskets
#' ggplot() +
#'     geom_posterior(design, n, r, aes(colour = basket))
geom_posterior.fujikawa <- function(design, n, r, ...) {
  purrr::pmap(cbind(t(beta_post(design, n, r)),
                    data.frame(basket = (1:design$k))),
              function(shape1post, shape2post, basket) {
                ggplot2::geom_function(
                  data = data.frame(basket = factor(basket, levels = (1:design$k))),
                  fun = dbeta,
                  args = list(shape1 = shape1post,
                              shape2 = shape2post),
                  ...
                )
              })
}
#' Plot a Bayesian basket trial's posterior distribution after borrowing
#'
#' @inherit geom_posterior params return
#' @inherit geom_borrow.fujikawa examples
#'
#' @export
geom_borrow <- function(design, ...) {
  UseMethod("geom_borrow")
}
#' Plot a Fujikawa basket trial's posterior distribution after borrowing
#'
#' @template dotdotdot_geom
#' @template design
#' @template n
#' @template r
#' @inherit geom_borrow  return
#'
#' @export
#'
#' @examples
#' # Example for a basket trial with Fujikawa's Design
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#' n <- 20
#' r <- c(4, 5, 2)
#' epsilon <- 2
#' tau <- 0.5
#' # One facet per basket
#' library(ggplot2)
#' ggplot() +
#'     geom_borrow(design, n, r, epsilon, tau, logbase = exp(1)) +
#'     facet_wrap(vars(basket))
#' # Colour different baskets
#' ggplot() +
#'     geom_borrow(aes(colour = basket), design, n, r, epsilon, tau,
#'                 logbase = exp(1))
geom_borrow.fujikawa <-
  function(design, n, r, epsilon, tau, logbase, ...) {
    weights <-
      get_weights_jsd(
        design = design,
        n = n,
        epsilon = epsilon,
        tau = tau,
        logbase = logbase
      )
    purrr::pmap(cbind(t(beta_borrow_fujikawa(design, n, r, weights)),
                      data.frame(basket = (1:design$k))),
                function(shape1post, shape2post, basket) {
                  ggplot2::geom_function(
                    data = data.frame(basket = factor(basket, levels = (1:design$k))),
                    fun = dbeta,
                    args = list(shape1 = shape1post,
                                shape2 = shape2post),
                    ...
                  )
    })
}
