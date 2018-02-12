#' Corrected Lasso
#'
#' Lasso (L1-regularization) for generalized linear models with measurement
#' error.
#'
#' @details Corrected version of the lasso for generalized linear models. The
#'   method does require an estimate of the measurement error covariance matrix.
#' @param W Design matrix, measured with error. Must be a numeric matrix.
#' @param y Vector of responses.
#' @param sigmaUU Covariance matrix of the measurement error.
#' @param family Response type. Character string of length 1. Possible values
#'   are "gaussian" and "binomial".
#' @param radii Vector containing the set of radii of the l1-ball onto which the
#'   solution is projected. If not provided, the algorithm will select an evenly
#'   spaced vector of 20 radii.
#' @param alpha Step size of the projected gradient descent algorithm. Default
#'   is 0.1.
#' @param maxits Maxium number of iterations of the project gradient descent
#'   algorithm for each radius. Default is 5000.
#' @return Returns a list containing a matrix whose columns represent the
#'   corrected beta estimates at each radius, as well as the vector of radii
#'   used.
#'
#' @references \insertRef{loh2012}{hdme}
#'
#' \insertRef{sorensen2015}{hdme}
#' @examples
#' # Example with linear regression
#' n <- 100 # Number of samples
#' p <- 50 # Number of covariates
#' X <- matrix(rnorm(n * p), nrow = n)  # True (latent) variables
#' sigmaUU <- diag(x = 0.2, nrow = p, ncol = p) # Measurement error covariance matrix (typically estimated by replicate measurements)
#' W <- X + rnorm(n, sd = diag(sigmaUU)) # Measurement matrix (this is the one we observe)
#' beta <- c(seq(from = 0.1, to = 1, length.out = 5), rep(0, p-5)) # Coefficient
#' y <- X %*% beta + rnorm(n, sd = 1) # Response
#' fit <- fit_corrected_lasso(W, y, sigmaUU, family = "gaussian") # Run the corrected lasso
#' plot(fit)
#'
#' # Binomial, logistic regression
#' n <- 1000 # Number of samples
#' p <- 50 # Number of covariates
#' X <- matrix(rnorm(n * p), nrow = n) # True (latent) variables
#' sigmaUU <- diag(x = 0.2, nrow = p, ncol = p) # Measurement error covariance matrix
#' W <- X + rnorm(n, sd = diag(sigmaUU)) # Measurement matrix (this is the one we observe)
#' logit <- function(x) (1+exp(-x))^(-1)
#' y <- rbinom(n, size = 1, prob = logit(X %*% c(rep(5, 5), rep(0, p-5)))) # Response
#' fit <- fit_corrected_lasso(W, y, sigmaUU, family = "binomial")
#' plot(fit)
#'
#'
#' @importFrom Rdpack reprompt
#'
#' @export
fit_corrected_lasso <- function(W, y, sigmaUU, family = c("gaussian", "binomial"),
                 radii = NULL, no_radii = 20, alpha = 0.1, maxits = 5000){
  family <- match.arg(family)


  if(!is.matrix(W)) {
    stop("X should be a matrix")
  }

  if(!is.numeric(W)) {
    stop("X should be a numeric matrix")
  }

  y <- drop(y)

  if(!is.numeric(y)) {
    stop("y should be a numeric vector")
  }

  if(!is.matrix(sigmaUU)) {
    stop("sigmaUU should be a matrix")
  }




  n <- nrow(W)
  p <- ncol(W)

  if(length(y) != n) {
    stop("The length of y should equal the number of rows in W")
  }

  if(!family %in% c("gaussian", "binomial")) {
    stop("Argment family must have value 'gaussian' or 'binmial'")
  }


  fit <- switch(family,
             "gaussian" = corrected_lasso_gaussian(W = W, y = y, sigmaUU = sigmaUU, radii = radii, no_radii = no_radii, alpha = alpha, maxits = maxits),
             "binomial" = corrected_lasso_binomial(W = W, y = y, sigmaUU = sigmaUU, radii = radii, no_radii = no_radii, alpha = alpha, maxits = maxits)
             )

  class(fit) <- c("corrected_lasso")

  return(fit)

}
