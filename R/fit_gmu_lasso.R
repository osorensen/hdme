#' Generalized Matrix Uncertainty Lasso
#'
#' @param W Design matrix, measured with error. Must be a numeric matrix.
#' @param y Vector of responses.
#' @param lambda Regularization parameter.
#' @param delta Additional regularization parameter, bounding the measurement
#'   error.
#' @param family Character string. Currently "binomial" and "poisson" are supported.
#' @param active_set Logical. Whether or not to use an active set strategy to
#'   speed up coordinate descent algorithm.
#'
#' @return Coefficient vector.
#'
#' @references \insertRef{rosenbaum2010}{hdme}
#'
#' \insertRef{sorensen2018}{hdme}
#'
fit_gmu_lasso <- function(W, y, lambda, delta, family = "binomial",
                          active_set = FALSE){

  if(family == "binomial") {
    dmu <- dlogit
  } else if(family == "poisson") {
    dmu <- dpois
  } else {
    stop("Currently only 'binmial' and 'possion' are supported arguments for family.")
  }



  # We assume the first column in W takes the intercept

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
      Wtilde <- c(sqrt(V))*W
      ztilde <- c(sqrt(V))*c(z)
      gamma <- delta*sqrt(sum(V^2))/2/sqrt(n)
      omega <- rep(0,p)
      omega[-1] <- sapply(2:p, function(x) { lambda + gamma * sum(abs(bOld[-x])) })

      bNew <- fit_mu_lasso(omega, gamma, Wtilde, ztilde, bOld, active_set)
      count <- count+1
      Diff1 <- sum(abs(bNew - bOld))
      Diff2 <- sum(abs(bNew - bOlder))
      print(paste("Diff1 = ", Diff1, ", Diff2 = ", Diff2, sep=""))
  }
  if(count >= maxit) print(paste("Did not converge"))
  return(bNew)
}
