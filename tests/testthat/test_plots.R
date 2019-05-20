# Tests of plotting functions

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
test_that("plot.gmu_lasso works", {
  expect_s3_class(plot(fit), "ggplot")
})

fit <- gmus(W, y, family = "binomial")
test_that("plot.gmus works", {
  expect_s3_class(plot(fit), "ggplot")
})

fit <- corrected_lasso(W, y, sigmaUU, family = "binomial")
test_that("plot.correct_lasso works", {
  expect_s3_class(plot(fit), "ggplot")
  expect_s3_class(plot(fit, "path"), "ggplot")
  expect_error(plot(fit, "coefficients"))
})

# cv_corrected_lasso
suppressWarnings(RNGversion("3.5.0"))
set.seed(1, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 100
p <- 50
X <- matrix(rnorm(n * p), nrow = n)
sigmaUU <- diag(x = 0.2, nrow = p, ncol = p)
W <- X + rnorm(n, sd = diag(sigmaUU))
beta <- c(seq(from = 0.1, to = 1, length.out = 5), rep(0, p-5))
y <- X %*% beta + rnorm(n, sd = 1)
fit <- cv_corrected_lasso(W, y, sigmaUU, family = "gaussian")

test_that("plot.cv_correct_lasso works", {
  expect_s3_class(plot(fit), "ggplot")
})
