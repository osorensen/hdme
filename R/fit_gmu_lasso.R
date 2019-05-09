#' Generalized Matrix Uncertainty Lasso
#'
#' @param W Design matrix, measured with error. Must be a numeric matrix.
#' @param y Vector of responses.
#' @param lambda Regularization parameter. If not set, lambda.min from
#'   glmnet::cv.glmnet is used.
#' @param delta Additional regularization parameter, bounding the measurement
#'   error.
#' @param family Character string. Currently "binomial" and "poisson" are
#'   supported.
#' @param active_set Logical. Whether or not to use an active set strategy to
#'   speed up coordinate descent algorithm.
#'
#' @return List object with intercept and coefficients at the values of lambda
#'   and delta specified, as well as regularization parameters.
#' @export
#'
#' @references \insertRef{rosenbaum2010}{hdme}
#'
#'   \insertRef{sorensen2018}{hdme}
#'
#' @examples
#' set.seed(1)
#' # Number of samples
#' n <- 200
#' # Number of covariates
#' p <- 100
#' # Number of nonzero features
#' s <- 10
#' # True coefficient vector
#' beta <- c(rep(1,s),rep(0,p-s))
#' # Standard deviation of measurement error
#' sdU <- 0.2
#' # True data, not observed
#' X <- matrix(rnorm(n*p),nrow = n,ncol = p)
#' # Measured data, with error
#' W <- X + sdU * matrix(rnorm(n * p), nrow = n, ncol = p)
#' # Binomial response
#' y <- rbinom(n, 1, (1 + exp(-X%*%beta))**(-1))
#' # Run the GMU Lasso
#' gmu_lasso <- fit_gmu_lasso(W, y, delta = NULL)
#' # Get an elbow plot, in order to choose delta.
#' plot(gmu_lasso)
#'
#'
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
  if(is.null(lambda)) lambda <- glmnet::cv.glmnet(W, y, family = family)$lambda.min
  if(is.null(delta)) delta <- seq(from = 0, to = 0.2, by = 0.05)

  n <- dim(W)[1]
  p <- dim(W)[2]
  bOld <- stats::rnorm(p)/p
  bNew <- stats::rnorm(p)/p
  IRLSeps <- 1e-7
  maxit <- 100


  bhatGMUL <- matrix(nrow=p, ncol=length(delta))

  for(i in seq_along(delta)) {
    d <- delta[i]

    count <- 1
    Diff1 <- 1
    Diff2 <- 1
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

  fit <- list(intercept = bhatGMUL[1, ],
              beta = bhatGMUL[-1, ] / scales,
              family = family,
              delta = delta,
              lambda = lambda,
              num_non_zero = colSums(bhatGMUL[-1, , drop = FALSE] > 0)
  )

  class(fit) <- "gmu_lasso"
  return(fit)
}
