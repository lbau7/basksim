#' Setup bma Design Object
#'
#' Creates an object of class \code{bma}.
#'
#' @template k
#' @template p0
#' @template shape_beta
#'
#' @return An S3 object of class \code{bma}
#' @export
#'
#' @details The class \code{bma} implements the Bayesian Model Averaging
#' design by Pisoda et al. (2021).
#'
#' @references Psioda, M. A., Xu, J., Jiang, Q. I., Ke, C., Yang, Z., &
#' Ibrahim, J. G. (2021). Bayesian adaptive basket trial design using model
#' averaging. Biostatistics, 22(1), 19-34.
#'
#' @examples
#' design_bma <- setup_bma(k = 3, p0 = 0.2)
setup_bma <- function(k, p0, shape1 = 0.5, shape2 = 0.5) {
  mu0 <- shape1 / (shape1 + shape2)
  phi0 <- shape1 + shape2
  structure(
    list(k = k, p0 = p0, mu0 = mu0, phi0 = phi0),
    class = "bma"
  )
}

#' Setup ebcomb Design Object
#'
#' Creates an object of class \code{ebcomb}.
#'
#' @template k
#' @template p0
#' @template shape_beta
#'
#' @details The class \code{ebcomb} implements an empirical Bayes method
#' by Gravestock & Held (2019) which was proposed for borrowing strength
#' from multiple external studies.
#'
#' @references Gravestock, I., & Held, L. (2019). Power priors based on
#' multiple historical studies for binary outcomes. Biometrical Journal, 61(5),
#' 1201-1218.
#'
#' @return An S3 object of class \code{bma}
#' @export
#'
#' @examples
#' design_bma <- setup_bma(k = 3, p0 = 0.2)
setup_ebcomb <- function(k, p0, shape1 = 1, shape2 = 1) {
  validate_betabin(structure(
    list(k = k, p0 = p0, shape1 = shape1, shape2 = shape2),
    class = "ebcomb"
  ))
}

#' Setup BHM Design Object
#'
#' @template k
#' @template p0
#' @template p_target
#' @template mu_bhm
#'
#' @details The class \code{bhm} implements the Bayesian Hierarchical Model
#' proposed by Berry et al. (2013). Methods for this class are
#' mostly wrappers for functions from the package \code{bhmbasket}.
#'
#' In the BHM the thetas of all baskets are modeled, where theta_i =
#' logit(p_i) - logit(p_target). These thetas are assumed to come from
#' a normal distribution with mean mu_mean and standard deviation mu_sd.
#' If \code{mu_mean = NULL} then mu_mean is determined as logit(p0) -
#' logit(p_target), hence the mean of the normal distribution corresponds
#' to the null hypothesis.
#'
#' @references Berry, S. M., Broglio, K. R., Groshen, S., & Berry, D. A. (2013).
#' Bayesian hierarchical modeling of patient subpopulations: efficient designs
#' of phase II oncology clinical trials. Clinical Trials, 10(5), 720-734.
#'
#' @return An S3 object of class \code{bhm}
#' @export
#'
#' @examples
#' design_bhm <- setup_bhm(k = 3, p0 = 0.2, p_target = 0.5)
setup_bhm <- function(k, p0, p_target, mu_mean = NULL, mu_sd = 100) {
  if (is.null(mu_mean)) mu_mean <- bhmbasket::logit(p0) -
      bhmbasket::logit(p_target)
  structure(
    list(k = k, p0 = p0, p_target = p_target, mu_mean = mu_mean, mu_sd = mu_sd),
    class = "bhm"
  )
}

#' Setup EXNEX Design Object
#'
#' @template k
#' @template p0
#' @template params_exnex
#'
#' @details The class \code{exnex} implements the EXNEX model proposed by
#' Neuenschwander et al. (2016). Methods for this class are mostly wrappers
#' for functions from the package \code{bhmbasket}.
#'
#' In the EXNEX model the thetas of all baskets are modeled as a mixture
#' of individual models and a Bayesian Hierarchical Model with a fixed
#' mixture weight w. If \code{mu_mean} and \code{basket_mean} are \code{NULL}
#' then they are set to logit(p0).
#' Note that Neuenschwander et al. (2016) use different prior means and
#' standard deviations. The default values here are used for better comparison
#' with the BHM model (see \code{\link{setup_bhm}}).
#'
#' @references Neuenschwander, B., Wandel, S., Roychoudhury, S., & Bailey, S.
#' (2016). Robust exchangeability designs for early phase clinical trials with
#' multiple strata. Pharmaceutical statistics, 15(2), 123-134.
#'
#' @return An S3 object of class \code{exnex}
#' @export
#'
#' @examples
#' design_exnex <- setup_exnex(k = 3, p0 = 0.2)
setup_exnex <- function(k, p0, basket_mean = NULL, basket_sd = 100,
                        mu_mean = NULL, mu_sd = 100) {
  if (is.null(basket_mean)) basket_mean <- bhmbasket::logit(p0)
  if (is.null(mu_mean)) mu_mean <- bhmbasket::logit(p0)
  structure(
    list(k = k, p0 = p0, basket_mean = basket_mean, basket_sd = basket_sd,
      mu_mean = mu_mean, mu_sd = mu_sd),
    class = "exnex"
  )
}

#' Setup Fujikawa Design Object
#'
#' @template k
#' @template p0
#' @template shape_beta
#'
#' @references Fujikawa, K., Teramukai, S., Yokota, I., & Daimon, T. (2020).
#' A Bayesian basket trial design that borrows information across strata based
#' on the similarity between the posterior distributions of the response
#' probability. Biometrical Journal, 62(2), 330-338.
#'
#' @return An S3 object of class \code{fujikawa}
#' @export
#'
#' @examples
#' design_fujikawa <- setup_fujikawa(k = 3, p0 = 0.2)
setup_fujikawa <- function(k, p0, shape1 = 1, shape2 = 1) {
  validate_betabin(structure(
    list(k = k, p0 = p0, shape1 = shape1, shape2 = shape2),
    class = "fujikawa"
  ))
}

#' Setup Generalized JSD Design Object
#'
#' @template k
#' @template p0
#' @template shape_beta
#'
#' @return An S3 object of class \code{jsdgen}
#' @export
#'
#' @examples
#' design_jsdgen <- setup_jsdgen(k = 3, p0 = 0.2)
setup_jsdgen <- function(k, p0, shape1 = 1, shape2 = 1) {
  validate_betabin(structure(
    list(k = k, p0 = p0, shape1 = shape1, shape2 = shape2),
    class = "jsdgen"
  ))
}

#' Setup Calibrated Power Prior Design Object
#'
#' @template k
#' @template p0
#' @template shape_beta
#'
#' @return An S3 object of class \code{cpp}
#' @export
#'
#' @examples
#' design_cpp <- setup_cpp(k = 3, p0 = 0.2)
setup_cpp <- function(k, p0, shape1 = 1, shape2 = 1) {
  validate_betabin(structure(
    list(k = k, p0 = p0, shape1 = shape1, shape2 = shape2),
    class = "cpp"
  ))
}

#' Setup Generalized Calibrated Power Prior Design Object
#'
#' @template k
#' @template p0
#' @template shape_beta
#'
#' @return An S3 object of class \code{cppgen}
#' @export
#'
#' @examples
#' design_cppgen <- setup_cppgen(k = 3, p0 = 0.2)
setup_cppgen <- function(k, p0, shape1 = 1, shape2 = 1) {
  validate_betabin(structure(
    list(k = k, p0 = p0, shape1 = shape1, shape2 = shape2),
    class = "cppgen"
  ))
}