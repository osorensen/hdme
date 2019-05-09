#' Generalized Matrix Uncertainty Selector for logistic regression
#'
#' @description Internal function.
#' @param W Design matrix, measured with error.
#' @param y Vector of the binomial response value.
#' @param lambda Regularization parameter due to model error.
#' @param delta Regularization parameter due to measurement error.
#' @param family "binomial" or "poisson"
#' @return Intercept and coefficients at the values of lambda and delta specified.
#'
#' @keywords internal
#'
mus_glm <- function(W, y, lambda, delta, family = c("binomial", "poisson")){

  family <- match.arg(family)

  if(family == "binomial") {
    mu <- logit
    dmu <- dlogit
  } else if(family == "poisson") {
    mu <- pois
    dmu <- dpois
  }


  n <- dim(W)[1]
  p <- dim(W)[2]

  bOld <- stats::rnorm(p)/p
  bNew <- stats::rnorm(p)/p
  IRLSeps <- 1e-7
  maxit <- 100
  count <- 1
  Diff1 <- 1
  Diff2 <- 1

  while(Diff1 > IRLSeps & Diff2 > IRLSeps & count < maxit){
      bOlder <- bOld
      bOld <- bNew
      V <- dmu(W%*%bOld)
      z <- W%*%bOld + (y - mu(W%*%bOld))/dmu(W%*%bOld)
      Wtilde <- c(sqrt(V)) * W
      ztilde <- c(sqrt(V)) * c(z)
      bNew <- musalgorithm(Wtilde, ztilde, lambda, delta * sqrt(sum((V)^2)) / sqrt(n))

      count <- count+1
      Diff1 <- sum(abs(bNew - bOld))
      Diff2 <- sum(abs(bNew - bOlder))
  }
  if(count >= maxit) print(paste("Did not converge"))
  return(bNew)
}
