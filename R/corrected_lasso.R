#'Corrected Lasso
#'
#'@description Lasso (L1-regularization) for generalized linear models with
#'  measurement error.
#'
#'@details Corrected version of the lasso for generalized linear models. The
#'  method does require an estimate of the measurement error covariance matrix.
#'  The Poisson regression option might sensitive to numerical overflow, please
#'  file a GitHub issue in the source repository if you experience this.
#'@param W Design matrix, measured with error. Must be a numeric matrix.
#'@param y Vector of responses.
#'@param sigmaUU Covariance matrix of the measurement error.
#'@param family Response type. Character string of length 1. Possible values are
#'  "gaussian", "binomial" and "poisson".
#'@param radii Vector containing the set of radii of the l1-ball onto which the
#'  solution is projected. If not provided, the algorithm will select an evenly
#'  spaced vector of 20 radii.
#'@param no_radii Length of vector radii, i.e., the number of regularization
#'  parameters to fit the corrected lasso for.
#'@param alpha Step size of the projected gradient descent algorithm. Default is
#'  0.1.
#'@param maxits Maximum number of iterations of the project gradient descent
#'  algorithm for each radius. Default is 5000.
#'@param tol Iteration tolerance for change in sum of squares of beta. Defaults
#'. to 1e-12.
#'@return An object of class "corrected_lasso".
#'
#'
#'@references \insertRef{loh2012}{hdme}
#'
#'\insertRef{sorensen2015}{hdme}
#'
#' @examples
#' # Example with linear regression
#' # Number of samples
#' n <- 100
#' # Number of covariates
#' p <- 50
#' # True (latent) variables
#' X <- matrix(rnorm(n * p), nrow = n)
#' # Measurement error covariance matrix
#' # (typically estimated by replicate measurements)
#' sigmaUU <- diag(x = 0.2, nrow = p, ncol = p)
#' # Measurement matrix (this is the one we observe)
#' W <- X + rnorm(n, sd = sqrt(diag(sigmaUU)))
#' # Coefficient
#' beta <- c(seq(from = 0.1, to = 1, length.out = 5), rep(0, p-5))
#' # Response
#' y <- X %*% beta + rnorm(n, sd = 1)
#' # Run the corrected lasso
#' fit <- corrected_lasso(W, y, sigmaUU, family = "gaussian")
#' coef(fit)
#' plot(fit)
#' plot(fit, type = "path")
#'
#' # Binomial, logistic regression
#' # Number of samples
#' n <- 1000
#' # Number of covariates
#' p <- 50
#' # True (latent) variables
#' X <- matrix(rnorm(n * p), nrow = n)
#' # Measurement error covariance matrix
#' sigmaUU <- diag(x = 0.2, nrow = p, ncol = p)
#' # Measurement matrix (this is the one we observe)
#' W <- X + rnorm(n, sd = sqrt(diag(sigmaUU)))
#' # Response
#' y <- rbinom(n, size = 1, prob = plogis(X %*% c(rep(5, 5), rep(0, p-5))))
#' fit <- corrected_lasso(W, y, sigmaUU, family = "binomial")
#' plot(fit)
#' coef(fit)
#'
#'@importFrom Rdpack reprompt
#'
#'@export
corrected_lasso <- function(W, y, sigmaUU, family = c("gaussian", "binomial", "poisson"),
                 radii = NULL, no_radii = NULL, alpha = 0.1, maxits = 5000, tol = 1e-12){
  family <- match.arg(family)


  if(!is.matrix(W)) {
    stop("W should be a matrix")
  }

  if(!is.numeric(W)) {
    stop("W should be a numeric matrix")
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

  if(family == "gaussian") {
    fit <- corrected_lasso_gaussian(W = W, y = y, sigmaUU = sigmaUU,
                                    radii = radii, no_radii = no_radii,
                                    alpha = alpha, maxits = maxits, tol = tol)
  } else if(family %in% c("binomial", "poisson")) {
    fit <- corrected_lasso_glm(W = W, y = y, sigmaUU = sigmaUU, family = family,
                               radii = radii, no_radii = no_radii,
                               alpha = alpha, maxits = maxits, tol = tol)
  }

  fit$family <- family

  class(fit) <- c("corrected_lasso")

  return(fit)

}
