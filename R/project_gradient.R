# Function which does the project gradient descent
# Based on a Matlab function developed by Po-Ling Loh and Martin J. Wainwright
#' @useDynLib hdme, .registration = TRUE
#' @importFrom Rcpp sourceCpp
project_gradient <- function(Q, b, maxits, alpha, R, betaOld) {
  its <- 1 # Number of iterations
  change <- 1 # Initial value, change per iteration
  tol <- 1e-4 # Tolerance in change of \beta, stopping criterion
  p <- length(b)
  while(its <= maxits & change > tol){
    grad <- Q %*% betaOld - b
    betaNew <- project_onto_l1_ball(betaOld - alpha*grad, R)
    diff <- sum(betaNew^2 - betaOld^2)
    change <- diff / alpha
    betaOld <- betaNew
    its <- its+1
  }
  if(its == maxits+1) print('Max iterations')
  return(betaNew)
}

