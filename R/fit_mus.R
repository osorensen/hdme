#' Matrix Uncertainty Selector
#' @description Matrix Uncertainty Selector
#' @import glmnet
#' @param X Design matrix.
#' @param y Vector of the response value.
#' @param lambda Regularization parameter.
#' @param delta Additional regularization parameter, bounding the measurement error.
#' @return Intercept and coefficients at the values of lambda and delta specified.
#' @references \insertRef{rosenbaum2010}{hdme}
#' \insertRef{sorensen2018}{hdme}
#' @examples
#' # Example with linear regression
#' set.seed(1)
#' n <- 1000 # Number of samples
#' p <- 200 # Number of covariates
#' X <- matrix(rnorm(n * p), nrow = n) # True (latent) variables
#' W <- X + matrix(rnorm(n*p, sd = 1), nrow = n, ncol = p) # Measurement matrix (this is the one we observe)
#' beta <- c(seq(from = 0.1, to = 1, length.out = 5), rep(0, p-5)) # Coefficient vector
#' y <- X %*% beta + rnorm(n, sd = 1) # Response
#' mus1 <- fit_mus(W, y) # Run the MU Selector
#' plot(fit) # Draw an elbow plot to select delta
#'
#' # Now, according to the "elbow rule", choose the final delta where the curve has an "elbow".
#' # In this case, the elbow is at about delta = 0.08, so we use this to compute the final estimate:
#' mus2 <- fit_mus(W, y, delta = 0.08)
#' plot(fit) # Plot the coefficients
#'
#' @export
fit_mus <- function(W, y, lambda = NULL, delta = NULL) {
  fit <- fit_gmus(W, y, lambda = lambda, delta = delta, family = "gaussian")
  return(fit)
}



