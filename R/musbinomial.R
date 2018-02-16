#' Generalized Matrix Uncertainty Selector for logistic regression
#'
#' @description Internal function.
#' @param W Design matrix, measured with error.
#' @param y Vector of the binomial response value.
#' @param lambda Regularization parameter due to model error.
#' @param delta Regularization parameter due to measurement error.
#' @return Intercept and coefficients at the values of lambda and delta specified.
#'  @import stats
musbinomial <- function(W, y, lambda, delta){

  mu <- logit
  dmu <- dlogit

  n <- dim(W)[1]
  p <- dim(W)[2]

  bOld <- rnorm(p)/p
  bNew <- rnorm(p)/p
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
