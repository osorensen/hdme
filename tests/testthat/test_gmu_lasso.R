# Tests of gmu_lasso

### Logistic regression
# Generate example data and create a first fit
suppressWarnings(RNGversion("3.5.0"))
set.seed(1, kind = "Mersenne-Twister", normal.kind = "Inversion")

n <- 1000  # Number of samples
p <- 10 # Number of covariates
X <- matrix(rnorm(n * p), nrow = n) # True (latent) variables # Design matrix
sigmaUU <- diag(x = 0.2, nrow = p, ncol = p)
W <- X + rnorm(n, sd = diag(sigmaUU))
beta <- c(seq(from = 0.1, to = 1, length.out = 5), rep(0, p-5)) # True regression coefficients
y <- rbinom(n, 1, (1 + exp(-X %*% beta))^(-1)) # Binomially distributed response
fit <- gmu_lasso(W, y, family = "binomial")

# Test that the result is as it should
test_that("gmu_lasso returns correct object", {
  expect_s3_class(fit, "gmu_lasso")
  expect_equal(fit$family, "binomial")
  expect_equal(dim(fit$beta), c(10, 26))
  expect_equal(round(fit$beta[3, 5], 7), 0.2340902)
  expect_equal(round(fit$beta[7, 1], 7), -0.0852176)
  expect_equal(length(fit$delta), 26)
  expect_equal(round(fit$lambda, 7), 0.0053673)
})

# Test that the S3 methods work
test_that("S3 methods for gmu_lasso work", {
  expect_output(coef(fit),
                regexp = "Number of nonzero coefficient estimates")
  expect_output(print(fit),
                regexp = "Generalized MU Lasso with family binomial, with 10 variables fitted with regularization parameters lambda = 0.00536")
  expect_s3_class(plot(fit), "ggplot")
})

# Next test that it fails when it should
test_that("gmu_lasso fails when it should", {
  expect_error(gmu_lasso(W, y, sigmaUU = sigmaUU))
  expect_error(gmu_lasso(W, y, family = "gamma"))
  expect_error(gmu_lasso(list(W), y))
  expect_error(gmu_lasso(W, y, lambda = -1))
  expect_error(gmu_lasso(W, y, delta = -1:3))
})


# Poisson regression
suppressWarnings(RNGversion("3.5.0"))
set.seed(3, kind = "Mersenne-Twister", normal.kind = "Inversion")


### Poisson regression
# Generate example data and create a first fit
suppressWarnings(RNGversion("3.5.0"))
set.seed(1, kind = "Mersenne-Twister", normal.kind = "Inversion")

n <- 100
p <- 15

# True (latent) variables
X <- matrix(rnorm(n * p), nrow = n)
# Measurement matrix (this is the one we observe)
W <- X + matrix(rnorm(n*p, sd = .2), nrow = n, ncol = p)
# Coefficient vector
beta <- c(rep(.2, 5), rep(0, p-5))
# Response
y <- rpois(n, exp(X %*% beta))
# Run the GMU Lasso
fit <- gmu_lasso(W, y, family = "poisson")


# Test that the result is as it should
test_that("gmu_lasso returns correct object", {
  expect_s3_class(fit, "gmu_lasso")
  expect_equal(fit$family, "poisson")
  expect_equal(dim(fit$beta), c(15, 26))
  expect_equal(round(fit$beta[3, 5], 7), 0)
  expect_equal(round(fit$beta[7, 1], 7), 0)
  expect_equal(round(fit$beta[5, 23], 7), 0.0633121)
  expect_equal(length(fit$delta), 26)
  expect_equal(round(fit$lambda, 7), 0.2194676)
})


# Test that the S3 methods work
test_that("S3 methods for gmus work", {
  expect_output(coef(fit),
                regexp = "Number of nonzero coefficient estimates")
  expect_output(print(fit),
                regexp = "Generalized MU Lasso with family poisson")
  expect_s3_class(plot(fit), "ggplot")
})


# Convergence
suppressWarnings(RNGversion("3.5.0"))
set.seed(1, kind = "Mersenne-Twister", normal.kind = "Inversion")

n <- 100  # Number of samples
p <- 100 # Number of covariates
X <- matrix(rnorm(n * p), nrow = n) # True (latent) variables # Design matrix
sigmaUU <- diag(x = 0.2, nrow = p, ncol = p)
W <- X + rnorm(n, sd = diag(sigmaUU))
beta <- rnorm(n, sd = 0.001)
y <- rbinom(n, 1, (1 + exp(-X %*% beta))^(-1)) # Binomially distributed response
test_that("lack of convergence causes error", {
  expect_error(gmu_lasso(W, y, family = "binomial", maxit = 2, active_set = FALSE))
})
