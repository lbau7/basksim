#' Plot a Bayesian basket trial's prior distribution
#'
#' @template dotdotdot_geom
#' @template design
#'
#' @return A list of ggplot layers of type `geom_function`.
#' @inherit geom_prior.fujikawa examples
#'
#' @export
geom_prior <- function(..., design) {
  UseMethod("geom_prior", ..., design)
}
#' Plot a Fujikawa basket trial's prior distribution
#'
#' @inherit geom_prior params return
#'
#' @export
#'
#' @examples
#' # Example for a basket trial with Fujikawa's Design
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#' # One facet per basket
#' ggplot() +
#'     geom_prior(design) +
#'     facet_wrap(vars(basket))
#' # Colour different baskets
#' ggplot() +
#'     geom_prior(aes(colour = basket), design)
geom_prior.fujikawa <- function(..., design) {
  purrr::pmap(data.frame(basket = (1:design$k)),
              function(basket) {
                geom_function(
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
#' @template dotdotdot_geom
#' @template design
#' @template n
#' @template r
#'
#' @return A list of ggplot layers of type `geom_function`.
#' @inherit geom_posterior.fujikawa examples
#'
#' @export
geom_posterior <- function(..., design, n, r) {
  UseMethod("geom_posterior", ..., design, n, r)
}
#' Plot a Fujikawa basket trial's posterior distribution
#'
#' @inherit geom_posterior params return
#'
#' @export
#'
#' @examples
#' # Example for a basket trial with Fujikawa's Design
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#' n <- 20
#' r <- c(4, 5, 2)
#' # One facet per basket
#' ggplot() +
#'     geom_posterior(design, n, r) +
#'     facet_wrap(vars(basket))
#' # Colour different baskets
#' ggplot() +
#'     geom_posterior(aes(colour = basket), design, n, r)
geom_posterior.fujikawa <- function(..., design, n, r) {
  purrr::pmap(cbind(t(beta_post(design, n, r)),
                    data.frame(basket = (1:design$k))),
              function(shape1post, shape2post, basket) {
                geom_function(
                  data = data.frame(basket = factor(basket, levels = (1:design$k))),
                  fun = dbeta,
                  args = list(shape1 = shape1post,
                              shape2 = shape2post),
                  ...
                )
              })
}
#' Plot a Bayesian basket trial's posterior distribution after applying borrowing
#'
#' @inherit geom_posterior params return
#' @inherit geom_borrow.fujikawa examples
#'
#' @export
geom_borrow <- function(..., design, n, r) {
  UseMethod("geom_posterior", ..., design, n, r)
}
#' Plot a Fujikawa basket trial's posterior distribution after borrowing
#'
#' @inherit geom_borrow params return
#'
#' @export
#'
#' @examples
#' # Example for a basket trial with Fujikawa's Design
#' design <- setup_fujikawa(k = 3, p0 = 0.2)
#' n <- 20
#' r <- c(4, 5, 2)
#' lambda <- 0.99
#' epsilon <- 2
#' tau <- 0.5
#' # One facet per basket
#' ggplot() +
#'     geom_borrow(design, n, r, lambda, epsilon, tau) +
#'     facet_wrap(vars(basket))
#' # Colour different baskets
#' ggplot() +
#'     geom_borrow(aes(colour = basket), design, n, r, lambda, epsilon, tau)
geom_borrow.fujikawa <-
  function(..., design, n, r, lambda, epsilon, tau) {
    weights <-
      get_weights_jsd(
        design = design,
        n = n,
        epsilon = epsilon,
        tau = tau
      )
    purrr::pmap(cbind(t(
      beta_borrow_fujikawa(design, n, r, weights)
    ),
    data.frame(basket = (1:design$k))),
    function(basket) {
      geom_function(
        ...,
        data = data.frame(basket = basket),
        fun = dbeta,
        args = list(shape1 = design$shape1,
                    shape2 = design$shape2)
      )
    })
  }
