library(hdme)

context("Testing corrected lasso")

n <- 100 # Number of samples
p <- 5 # Number of covariates
X <- matrix(rnorm(n * p), nrow = n)  # True (latent) variables
sigmaUU <- diag(x = 0.2, nrow = p, ncol = p) # Measurement error covariance matrix (typically estimated by replicate measurements)
W <- X + rnorm(n, sd = diag(sigmaUU)) # Measurement matrix (this is the one we observe)
beta <- c(seq(from = 0.1, to = 1, length.out = 5), rep(0, p-5)) # Coefficient
y <- X %*% beta + rnorm(n, sd = 1) # Response
fit <- fit_corrected_lasso(W, y, sigmaUU, family = "gaussian", no_radii = 2) # Run the corrected lasso

test_that("fit_corrected_lasso returns right object", {
  expect_output(str(fit), "List of 2")
  expect_equal(class(fit), "corrected_lasso")
})

cv_fit <- cv_corrected_lasso(W, y, sigmaUU, n_folds = 2, radii = 2)

test_that("cv_corrected_lasso returns right object", {
  expect_output(str(cv_fit), "List of 5")
  expect_equal(class(cv_fit), "cv_corrected_lasso")
})
