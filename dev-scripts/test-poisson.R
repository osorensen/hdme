library(hdme)
set.seed(123)
n <- 100
p <- 6
q <- 2

coefs <- vapply(1:100, function(i){
  X <- matrix(rnorm(n * p), nrow = n)
  sigmaUU <- diag(x = 0.2, nrow = p, ncol = p)
  W <- X + rnorm(n, sd = sqrt(diag(sigmaUU)))
  y <- rpois(n, exp(X %*% c(rep(.3, q), rep(0, p-q))))
  fit <- corrected_lasso(W, y, sigmaUU, family = "poisson")
  fit$betaCorr[, ncol(fit$betaCorr)]
}, FUN.VALUE = numeric(p))


apply(coefs, 1, function(x) sum(x != 0))
apply(coefs, 1, function(x) mean(x))
