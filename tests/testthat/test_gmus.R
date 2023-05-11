# Tests of gmus

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
set.seed(1, kind = "Mersenne-Twister", normal.kind = "Inversion")
fit <- gmus(W, y, family = "gaussian")
set.seed(1, kind = "Mersenne-Twister", normal.kind = "Inversion")
fit2 <- mus(W, y)
test_that("mus function works", {
  expect_equal(fit, fit2)
})

# Test that the result is as it should
test_that("gmus returns correct object", {
  expect_s3_class(fit, "gmus")
  expect_equal(fit$family, "gaussian")
  expect_equal(dim(fit$beta), c(50, 26))
  expect_equal(round(fit$beta[3, 5], 7), 0.4646516)
  expect_equal(round(fit$beta[13, 15], 7), 0)
  expect_equal(length(fit$delta), 26)
  expect_equal(round(fit$lambda, 7), 0.0672737)
})

# Next test that it fails when it should
test_that("gmus fails when it should", {
  expect_error(gmus(W, y, sigmaUU = sigmaUU))
  expect_error(gmus(W, y, family = "gamma"))
  expect_error(gmus(list(W), y))
  expect_error(gmus(W, y, lambda = -1))
  expect_error(gmus(W, y, delta = -1:3))
})

# Test that the S3 methods work
test_that("S3 methods for gmus work", {
  expect_output(coef(fit),
                regexp = "Number of nonzero coefficient estimates")
  expect_output(print(fit),
                regexp = "Generalized MU Selector with family gaussian")
  expect_s3_class(plot(fit), "ggplot")
})



## Test again with a single delta
set.seed(1, kind = "Mersenne-Twister", normal.kind = "Inversion")
fit <- gmus(W, y, delta = 0.2, family = "gaussian")
set.seed(1, kind = "Mersenne-Twister", normal.kind = "Inversion")
fit2 <- mus(W, y, delta = 0.2)
test_that("mus function works", {
  expect_equal(fit, fit2)
})

# Test that the result is as it should
test_that("gmus returns correct object", {
  expect_equal(dim(fit$beta), c(50, 1))
  expect_equal(round(fit$beta[3, 1], 7), 0.3711348)
  expect_equal(round(fit$beta[13, 1], 7), 0)
  expect_equal(length(fit$delta), 1)
  expect_equal(round(fit$lambda, 7), 0.0672737)
})

# Test that the S3 methods work
test_that("S3 methods for gmus work", {
  expect_output(coef(fit),
                regexp = "Non-zero coefficient estimates at")
  expect_output(coef(fit, all = TRUE),
                regexp = "Coefficient estimates at regularization")
  expect_output(print(fit),
                regexp = "Generalized MU Selector with family gaussian")
  expect_s3_class(plot(fit), "ggplot")
})

### Logistic regression
# Generate example data and create a first fit
suppressWarnings(RNGversion("3.5.0"))
set.seed(1, kind = "Mersenne-Twister", normal.kind = "Inversion")

n <- 1000  # Number of samples
p <- 10 # Number of covariates
X <- matrix(rnorm(n * p), nrow = n) # True (latent) variables # Design matrix
sigmaUU <- diag(x = 0.2, nrow = p, ncol = p)
W <- X + rnorm(n, sd = sqrt(diag(sigmaUU)))
beta <- c(seq(from = 0.1, to = 1, length.out = 5), rep(0, p-5)) # True regression coefficients
y <- rbinom(n, 1, (1 + exp(-X %*% beta))^(-1)) # Binomially distributed response
fit <- gmus(W, y, family = "binomial", delta = c(0, .1, .5))

# Test that the result is as it should
test_that("gmus returns correct object", {
  expect_s3_class(fit, "gmus")
  expect_equal(fit$family, "binomial")
  expect_equal(dim(fit$beta), c(10, 3))
  expect_equal(fit$beta,
               structure(c(-0.00902717310561197, 0.291585708663808, 0.22453144550476,
                           0.595950901714361, 0.697393684858912, -0.101859995760623, -0.180734619436561,
                           -0.135245313680128, -0.193714329179273, -0.168947345363323, 0,
                           0.126427485685824, 0.0613982726133604, 0.395632383186917, 0.461659975169094,
                           -1.98338549491951e-17, -0.026507087494231, -0.00949717831171686,
                           -0.0444942103781876, -0.0162802316357272, 0, 0, 0, 0.238425364798653,
                           0.288688356244267, 0, 0, 0, 9.06147456367423e-17, 0), dim = c(10L,
                                                                                         3L)))
  expect_equal(length(fit$delta), 3)
  expect_equal(fit$lambda, 0.00319864842791345)
})


# Test that the S3 methods work
test_that("S3 methods for gmus work", {
  expect_output(coef(fit),
                regexp = "Number of nonzero coefficient estimates")
  expect_output(print(fit),
                regexp = "Generalized MU Selector with family binomial")
  expect_s3_class(plot(fit), "ggplot")
})


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
# Run the MU Selector
fit <- gmus(W, y, family = "poisson")


# Test that the result is as it should
test_that("gmus returns correct object", {
  expect_s3_class(fit, "gmus")
  expect_equal(fit$family, "poisson")
  expect_equal(dim(fit$beta), c(15, 26))
  expect_equal(round(fit$beta[3, 5], 7), 0)
  expect_equal(round(fit$beta[7, 1], 7), 0)
  expect_equal(length(fit$delta), 26)
  expect_equal(round(fit$lambda, 7), 0.2194676)
})


# Test that the S3 methods work
test_that("S3 methods for gmus work", {
  expect_output(coef(fit),
                regexp = "Number of nonzero coefficient estimates")
  expect_output(print(fit),
                regexp = "Generalized MU Selector with family poisson")
  expect_s3_class(plot(fit), "ggplot")
})


