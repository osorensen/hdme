# Tests of cv_corrected_lasso

suppressWarnings(RNGversion("3.5.0"))
set.seed(1, kind = "Mersenne-Twister", normal.kind = "Inversion")

# Generate example data and create a first fit
n <- 100
p <- 50
X <- matrix(rnorm(n * p), nrow = n)
sigmaUU <- diag(x = 0.2, nrow = p, ncol = p)
W <- X + rnorm(n, sd = sqrt(diag(sigmaUU)))
beta <- c(seq(from = 0.1, to = 1, length.out = 5), rep(0, p-5))
y <- X %*% beta + rnorm(n, sd = 1)
fit <- cv_corrected_lasso(W, y, sigmaUU, family = "gaussian")

# First test that the result is as it should
test_that("cv_corrected_lasso returns correct object", {
  expect_s3_class(fit, "cv_corrected_lasso")
  expect_equal(fit$family, "gaussian")
  expect_equal(round(fit$radius_min, 6), 4.513754)
  expect_equal(round(fit$radius_1se, 6), 2.680729)
  expect_equal(round(fit$loss_min, 6), 1.046644)
  expect_equal(round(fit$loss_1se, 6), 1.264022)
})

# Next test that it fails when it should
test_that("cv_corrected_lasso fails when it should", {
  expect_error(cv_corrected_lasso(W, y))
  expect_error(cv_corrected_lasso(W, y, sigmaUU, family = "gamma"))
  expect_error(cv_corrected_lasso(W, y, sigmaUU, family = "binomial"))
  expect_error(corrected_lasso(list(W), y, sigmaUU))
  expect_error(corrected_lasso(W, y, sigmaUU, radii = -1))
  expect_error(corrected_lasso(W, y, sigmaUU, alpha = 0))
})

# Test that the S3 methods work
test_that("S3 methods for cv_corrected_lasso work", {
  expect_output(print(fit), regexp = "Cross-validation results:")
  expect_s3_class(plot(fit), "ggplot")
})

