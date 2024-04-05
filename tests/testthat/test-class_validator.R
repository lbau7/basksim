test_that("setup_fujikawa works", {
  design <- setup_fujikawa(k = 3, p0 = 0.2)
  expect_s3_class(design, "fujikawa")
})

test_that("setup_app works", {
  design <- setup_app(k = 3, p0 = 0.2)
  expect_s3_class(design, "app")
})

test_that("setup_cpp works", {
  design <- setup_cpp(k = 3, p0 = 0.2)
  expect_s3_class(design, "cpp")
})

test_that("setup_cpplim works", {
  design <- setup_cpplim(k = 3, p0 = 0.2)
  expect_s3_class(design, "cpplim")
})

test_that("errors in setup functions work", {
  expect_error(setup_fujikawa(k = 0, p0 = 0.2))
  expect_error(setup_cpp(k = 2.5, p0 = 0.5))
  expect_error(setup_app(k = 3.5, p0 = 0.5))
  expect_error(setup_cpplim(k = 4, p0 = 0.2, shape1 = 0, shape2 = 0))
})
