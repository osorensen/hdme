context("Testing Generalized Matrix Uncertainty Selector")

suppressWarnings(RNGversion("3.5.0"))
set.seed(100)
n <- 100 # Number of samples
p <- 5 # Number of covariates
X <- matrix(rnorm(n * p), nrow = n)  # True (latent) variables
sigmaUU <- diag(x = 0.2, nrow = p, ncol = p) # Measurement error covariance matrix (typically estimated by replicate measurements)
W <- X + rnorm(n, sd = diag(sigmaUU)) # Measurement matrix (this is the one we observe)
beta <- c(seq(from = 0.1, to = 1, length.out = 5), rep(0, p-5)) # Coefficient
y <- X %*% beta + rnorm(n, sd = 1) # Response
fit <- fit_gmus(W, y, family = "gaussian") # Run the GMUS

test_that("fit_gmus returns right object", {
  expect_output(str(fit), "List of 6")
  expect_equal(class(fit), "gmus")
})

# Try the shorthand fit_mus
fit2 <- fit_mus(W, y)
test_that("fit_mus returns right object", {
  expect_output(str(fit2), "List of 6")
  expect_equal(class(fit2), "gmus")
})

# Now check the logistic version
y <- rbinom(n, size = 1, prob = hdme:::logit(X %*% c(rep(5, 2), rep(0, p-2))))
deltavec <- seq(from = 0, to = 0.5, length.out = 10)
fit <- fit_gmus(W, y, family = "binomial", delta = deltavec, lambda = 0.001)

test_that("fit_gmus returns right object for logistic regression", {
  expect_output(str(fit), "List of 6")
  expect_equal(class(fit), "gmus")
})

test_that("gmus has right dimension in beta vectors", {
          expect_equal(dim(fit$beta), c(ncol(W), length(deltavec)))
          expect_equal(length(fit$delta), length(deltavec))
          expect_equal(fit$lambda, 0.001)
          })
