# Tests of corrected_lasso

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
fit <- corrected_lasso(W, y, sigmaUU, family = "gaussian")

# First test that the result is as it should
test_that("corrected_lasso returns correct object", {
  expect_s3_class(fit, "corrected_lasso")
  expect_equal(fit$family, "gaussian")
  expect_equal(dim(fit$betaCorr), c(50, 20))
  expect_equal(fit$betaCorr[3, 5], 0.477261461287873)
  expect_equal(fit$betaCorr[13, 15], -0.208150042972837)
  expect_equal(length(fit$radii), 20)
})

# Next test that it fails when it should
test_that("corrected_lasso fails when it should", {
  expect_error(corrected_lasso(W, y))
  expect_error(corrected_lasso(as.matrix(as.character(W)), y, sigmaUU))
  expect_error(corrected_lasso(W, as.character(y), sigmaUU))
  expect_error(corrected_lasso(W, y, list(sigmaUU)))
  expect_error(corrected_lasso(W, c(y, 2), sigmaUU, family = "gaussian"))
  expect_error(corrected_lasso(W, y, sigmaUU, family = "gamma"))
  expect_error(corrected_lasso(list(W), y, sigmaUU))
  expect_error(corrected_lasso(W, y, sigmaUU, radii = -1))
  expect_error(corrected_lasso(W, y, sigmaUU, alpha = 0))
})

# Test that the S3 methods work
test_that("S3 methods for corrected_lasso work", {
  expect_output(coef(fit),
                regexp = "Number of nonzero coefficient estimates")
  expect_output(print(fit),
                regexp = "Corrected gaussian lasso object with 50 variables fitted with 20 regularization parameters.")
  expect_s3_class(plot(fit), "ggplot")
  expect_s3_class(plot(fit, type = "path"), "ggplot")
})

# Fit again with a single regularization parameter.
# Different behavior is expected in this case
fit <- corrected_lasso(W, y, sigmaUU, family = "gaussian", radii = 2)

# Test again that the S3 methods work
test_that("S3 methods for corrected_lasso work", {
  expect_output(coef(fit),
                regexp = "Non-zero coefficients:")
  expect_output(print(fit),
                regexp = "Corrected gaussian lasso object with 50 variables fitted with 1 regularization parameters.")
  expect_s3_class(plot(fit), "ggplot")
  expect_s3_class(plot(fit, type = "path"), "ggplot")
  expect_message(plot(fit), regexp = "Only one regularization parameter. Plotting all coefficients.")
})


# Binomial, logistic regression
set.seed(1)
n <- 1000
p <- 50
X <- matrix(rnorm(n * p), nrow = n)
sigmaUU <- diag(x = 0.2, nrow = p, ncol = p)
W <- X + rnorm(n, sd = sqrt(diag(sigmaUU)))
y <- rbinom(n, size = 1, prob = plogis(X %*% c(rep(5, 5), rep(0, p-5))))
fit <- corrected_lasso(W, y, sigmaUU, family = "binomial")

# First test that the result is as it should
test_that("corrected_lasso returns correct object in the binomial case", {
  expect_s3_class(fit, "corrected_lasso")
  expect_equal(fit$family, "binomial")
  expect_equal(dim(fit$betaCorr), c(50, 20))
  expect_equal(round(fit$betaCorr[3, 5], 6), 0)
  expect_equal(round(fit$betaCorr[13, 15], 6), 0)
  expect_equal(length(fit$radii), 20)
})

# Test that the S3 methods work
test_that("S3 methods for corrected_lasso work", {
  expect_output(coef(fit),
                regexp = "Number of nonzero coefficient estimates")
  expect_output(print(fit),
                regexp = "Corrected binomial lasso object with 50 variables fitted with 20 regularization parameters.")
  expect_s3_class(plot(fit), "ggplot")
  expect_s3_class(plot(fit, type = "path"), "ggplot")
})



# Poisson regression
suppressWarnings(RNGversion("3.5.0"))
set.seed(3, kind = "Mersenne-Twister", normal.kind = "Inversion")

n <- 100
p <- 5
beta <- c(.01, .01, 0, 0, 0)
X <- matrix(rnorm(n * p), nrow = n)
sigmaUU <- diag(x = 0.2, nrow = p, ncol = p)
W <- X + rnorm(n, sd = sqrt(diag(sigmaUU)))
y <- rpois(n, exp(X %*% beta))
fit <- corrected_lasso(W, y, sigmaUU, family = "poisson")

# First test that the result is as it should
test_that("corrected_lasso returns correct object in the poisson case", {
  expect_s3_class(fit, "corrected_lasso")
  expect_equal(fit$family, "poisson")
  expect_equal(dim(fit$betaCorr), c(5, 20))
  expect_equal(round(fit$betaCorr[3, 5], 6), 0.006223)
  expect_equal(round(fit$betaCorr[1, 15], 6), 0)
  expect_equal(length(fit$radii), 20)
})

# Test that the S3 methods work
test_that("S3 methods for corrected_lasso work", {
  expect_output(print(fit),
                regexp = "Corrected poisson lasso object with 5 variables fitted with 20 regularization parameters.")
  expect_s3_class(plot(fit), "ggplot")
  expect_s3_class(plot(fit, type = "path"), "ggplot")
})

# Test for numerical overflow issue
test_that("Poisson regression tackles fairly big numbers", {
  n <- 1000
  p <- 50
  q <- 2
  beta <- 0.3

  X <- matrix(rnorm(n * p), nrow = n)
  sigmaUU <- diag(x = 0.2, nrow = p, ncol = p)
  W <- X + rnorm(n, sd = sqrt(diag(sigmaUU)))
  y <- rpois(n, exp(X %*% c(rep(beta, q), rep(0, p-q))))
  fit <- corrected_lasso(W, y, sigmaUU, family = "poisson")
  expect_s3_class(fit, "corrected_lasso")
})
