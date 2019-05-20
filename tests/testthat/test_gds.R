# Tests of gds

suppressWarnings(RNGversion("3.5.0"))
set.seed(1, kind = "Mersenne-Twister", normal.kind = "Inversion")

### Logistic GDS
# Generate example data and create a first fit
n <- 1000  # Number of samples
p <- 10 # Number of covariates
X <- matrix(rnorm(n * p), nrow = n) # True (latent) variables # Design matrix
beta <- c(seq(from = 0.1, to = 1, length.out = 5), rep(0, p-5)) # True regression coefficients
y <- rbinom(n, 1, (1 + exp(-X %*% beta))^(-1)) # Binomially distributed response

fit <- gds(X, y, family = "binomial")

# First test that the result is as it should
test_that("gds returns correct object", {
  expect_s3_class(fit, "gds")
  expect_equal(fit$family, "binomial")
  expect_equal(length(fit$beta), 10)
  expect_equal(round(fit$beta[[1]], 7), 0.1230598)
  expect_equal(round(fit$beta[[3]], 7), 0.5015788)
  expect_equal(round(fit$beta[[10]], 7), -0.0454783)
  expect_equal(round(fit$intercept, 7), -0.1187412)
  expect_equal(fit$num_non_zero, 8)
})

# Next test that it fails when it should
test_that("gds fails when it should", {
  expect_error(gds(X))
  expect_error(gds(X, lambda = 1:10))
  expect_error(gds(X, y, family = "gamma"))
  expect_error(gds(list(X), y))
  expect_error(gds(X, y, lambda = -1))
})

# Test that the S3 methods work
test_that("S3 methods for gds work", {
  expect_output(coef(fit),
                regexp = "Non-zero coefficients:")
  expect_output(print(fit),
                regexp = "Generalized Dantzig Selector with family binomial, with 10 variables fitted with regularization parameter")
  expect_s3_class(plot(fit), "ggplot")
})



### Gaussian GDS
suppressWarnings(RNGversion("3.5.0"))
set.seed(1, kind = "Mersenne-Twister", normal.kind = "Inversion")
# Generate example data and create a first fit
n <- 1000  # Number of samples
p <- 50 # Number of covariates
X <- matrix(rnorm(n * p), nrow = n) # True (latent) variables # Design matrix
beta <- c(seq(from = 0.1, to = 1, length.out = 5), rep(0, p-5)) # True regression coefficients
y <- X %*% beta + rnorm(n, sd = 0.5)

set.seed(1, kind = "Mersenne-Twister", normal.kind = "Inversion")
fit <- gds(X, y)
set.seed(1, kind = "Mersenne-Twister", normal.kind = "Inversion")
fit2 <- gds(X, y, family = "gaussian")

test_that("default family of gds works",
          expect_equal(fit, fit2))
rm(fit2)

# Test that the result is as it should
test_that("gds returns correct object", {
  expect_s3_class(fit, "gds")
  expect_equal(fit$family, "gaussian")
  expect_equal(length(fit$beta), 50)
  expect_equal(round(fit$beta[[1]], 7), 0.1056266)
  expect_equal(round(fit$beta[[30]], 7), 0)
  expect_equal(fit$num_non_zero, 13)
})

# Test that the S3 methods work also in this case
test_that("S3 methods for gds work", {
  expect_output(coef(fit),
                regexp = "Non-zero coefficients:")
  expect_output(coef(fit, all = TRUE), regexp = "Coefficient estimates:")
  expect_output(print(fit),
                regexp = "Generalized Dantzig Selector with family gaussian")
  expect_s3_class(plot(fit), "ggplot")
})


### Poisson GDS
suppressWarnings(RNGversion("3.5.0"))
set.seed(1, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 50
p <- 15

# True (latent) variables
X <- matrix(rnorm(n * p), nrow = n)
# Coefficient vector
beta <- c(rep(.2, 5), rep(0, p-5))
# Response
y <- rpois(n, exp(X %*% beta))
# Run the MU Selector
fit <- gds(X, y, family = "poisson")


# Test that the result is as it should
test_that("gds returns correct object", {
  expect_s3_class(fit, "gds")
  expect_equal(fit$family, "poisson")
  expect_equal(length(fit$beta), 15)
  expect_equal(round(fit$beta[[1]], 7), 0)
  expect_equal(round(fit$beta[[3]], 7), 0.3465382)
  expect_equal(fit$num_non_zero, 2)
})

# Test that the S3 methods work also in this case
test_that("S3 methods for gds work", {
  expect_output(coef(fit),
                regexp = "Non-zero coefficients:")
  expect_output(coef(fit, all = TRUE), regexp = "Coefficient estimates:")
  expect_output(print(fit),
                regexp = "Generalized Dantzig Selector with family poisson")
  expect_s3_class(plot(fit), "ggplot")
})
