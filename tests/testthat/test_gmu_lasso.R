# Tests of gmu_lasso

### Logistic regression
# Generate example data and create a first fit
suppressWarnings(RNGversion("3.5.0"))
set.seed(1)

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
                regexp = "Number of nonzero coefficient estimates as a function of regularization parameters")
  expect_output(print(fit),
                regexp = "Generalized MU Lasso with family binomial, with 10 variables")
  expect_s3_class(plot(fit), "ggplot")
})

# Next test that it fails when it should
test_that("gmu_lasso fails when it should", {
  expect_error(gmu_lasso(W, y, sigmaUU = sigmaUU))
  expect_error(gmu_lasso(W, y, family = "gamma"))
  expect_error(gmu_lasso(list(W), y))
  expect_error(gmu_lasso(W, y, lambda = -1))
  expect_error(gmus(W, y, delta = -1:3))
})
