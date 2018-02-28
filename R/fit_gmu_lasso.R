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
#' @examples
#' n <- 200
#' p <- 50
#' s <- 10
#' beta <- c(rep(1,s),rep(0,p-s))
#' sdU <- 0.2
#'
#' X <- matrix(rnorm(n*p),nrow = n,ncol = p)
#' W <- X + sdU * matrix(rnorm(n * p), nrow = n, ncol = p)
#'
#' y <- rbinom(n, 1, (1 + exp(-X%*%beta))**(-1))
#' bNew <- fit_gmu_lasso(W, y)
#'
#'
#' @import glmnet
fit_gmu_lasso <- function(W, y, lambda = NULL, delta = NULL,
                          family = "binomial", active_set = TRUE){

  if(family == "binomial") {
    mu <- logit
    dmu <- dlogit
  } else if(family == "poisson") {
    mu <- pois
    dmu <- dpois
  } else {
    stop("Currently only 'binomial' and 'possion' are supported arguments for family.")
  }

  # Standardize W
  W <- scale(W)
  scales <- attr(W, "scaled:scale")
  # Add intercept in first column
  W <- cbind(rep(1,n), W)

  # Run the lasso with cross validation to find a value for lambda
  if(is.null(lambda)) lambda <- cv.glmnet(W, y, family = family)$lambda.min
  if(is.null(delta)) delta <- seq(from = 0, to = 0.2, by = 0.05)

  n <- dim(W)[1]
  p <- dim(W)[2]
  bOld <- rnorm(p)/p
  bNew <- rnorm(p)/p
  IRLSeps <- 1e-7
  maxit <- 100
  count <- 1
  Diff1 <- 1
  Diff2 <- 1

  bhatGMUL <- matrix(nrow=p, ncol=length(delta))

  for(i in seq_along(delta)) {
    d <- delta[i]
    while(Diff1 > IRLSeps & Diff2 > IRLSeps & count < maxit){
      bOlder <- bOld
      bOld <- bNew
      V <- dmu(W %*% bOld)
      z <- W %*% bOld + (y - mu(W %*% bOld)) / dmu(W %*% bOld)
      Wtilde <- c(sqrt(V))*W
      ztilde <- c(sqrt(V))*c(z)
      gamma <- d*sqrt(sum(V^2))/2/sqrt(n)
      omega <- rep(0,p)
      omega[-1] <- sapply(2:p, function(x) { lambda + gamma * sum(abs(bOld[-x])) })

      bNew <- fit_mu_lasso(omega, gamma, Wtilde, ztilde, bOld, active_set)
      count <- count+1
      Diff1 <- sum(abs(bNew - bOld))
      Diff2 <- sum(abs(bNew - bOlder))
      #print(paste("Diff1 = ", Diff1, ", Diff2 = ", Diff2, sep=""))
    }
    if(count >= maxit) print(paste("Did not converge"))
    bhatGMUL[ ,i] <- bNew
  }
  ## TODO: Should return a list, including regularization parameters
  return(bhatGMUL)
}
