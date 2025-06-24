design <- setup_fujikawa(k = 3, p0 = 0.2)
n <- 20
r <- c(4, 5, 2)
test_that("geom_prior() works", {
  plot1 <- ggplot2::ggplot() +
    geom_prior(design) +
    ggplot2::facet_wrap(ggplot2::vars(basket))
  expect_s3_class(plot1$layers[[1]]$geom, "GeomFunction")
  expect_equal(plot1$layers[[1]]$data, data.frame(basket = 1))
})

test_that("geom_posterior() works", {
  plot1 <- ggplot2::ggplot() +
    geom_posterior(design,n, r) +
    ggplot2::facet_wrap(ggplot2::vars(basket))
  expect_s3_class(plot1$layers[[1]]$geom, "GeomFunction")
  expect_equal(plot1$layers[[1]]$data,
               data.frame(basket = factor(1, level = c("1", "2", "3"))))
})

test_that("geom_borrow() works", {
  plot1 <- ggplot2::ggplot() +
    geom_borrow(design, n, r,
                epsilon = 2, tau = 0.5, logbase = exp(1)) +
    ggplot2::facet_wrap(ggplot2::vars(basket))
  expect_s3_class(plot1$layers[[1]]$geom, "GeomFunction")
  expect_equal(plot1$layers[[1]]$data,
               data.frame(basket = factor(1, level = c("1", "2", "3"))))
})
