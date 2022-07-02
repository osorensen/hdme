# Tests of cv_corrected_lasso

suppressWarnings(RNGversion("3.5.0"))
set.seed(1, kind = "Mersenne-Twister", normal.kind = "Inversion")

### Logistic GDS
# Generate example data and create a first fit
n <- 1000  # Number of samples
p <- 10 # Number of covariates
X <- matrix(rnorm(n * p), nrow = n) # True (latent) variables # Design matrix
beta <- c(seq(from = 0.1, to = 1, length.out = 5), rep(0, p-5)) # True regression coefficients
y <- rbinom(n, 1, plogis(X %*% beta)) # Binomially distributed response

fit <- cv_gds(X, y)

# First test that the result is as it should
test_that("cv_gds returns correct object", {
  expect_s3_class(fit, "cv_gds")
  expect_equal(fit$family, "gaussian")
  expect_equal(round(fit$lambda_min, 6), 0.008077)
  expect_equal(round(fit$lambda_1se, 6), 0.022474)
  expect_equal(round(fit$loss_min, 3), 0.183)
  expect_equal(round(fit$loss_1se, 3), 0.185)
})

# Next test that it fails when it should
test_that("cv_gds fails when it should", {
  expect_error(cv_gds(X))
  expect_error(cv_gds(X, y, family = "cox"))
  expect_error(cv_gds(X, y, family = "gamma"))
  expect_error(cv_gds(list(X), y))
  expect_error(cv_gds(X, y, lambda = -1))
  expect_error(cv_gds(X, y, lambda = c(-1, 0, 2)))
  expect_error(cv_gds(X, y, alpha = 0))
})

# Test that the S3 methods work
test_that("S3 methods for cv_corrected_lasso work", {
  expect_output(print(fit), regexp = "Cross-validation results:")
  expect_s3_class(plot(fit), "ggplot")
})

