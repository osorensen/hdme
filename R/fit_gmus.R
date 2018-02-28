#' Generalized Matrix Uncertainty Selector
#' @description Generalized Matrix Uncertainty Selector
#' @import glmnet
#' @param W Design matrix, measured with error. Must be a numeric matrix.
#' @param y Vector of responses.
#' @param lambda Regularization parameter.
#' @param delta Additional regularization parameter, bounding the measurement
#'   error.
#' @param family "gaussian" for linear regression, "binomial" for logistic
#'   regression or "poisson" for Poisson regression.
#' @return List object with intercept and coefficients at the values of lambda
#'   and delta specified, as well as regularization parameters.
#' @references \insertRef{rosenbaum2010}{hdme}
#'
#'   \insertRef{sorensen2018}{hdme}
#' @examples
#' # Example with linear regression
#' set.seed(1)
#' n <- 100 # Number of samples
#' p <- 50 # Number of covariates
#' # True (latent) variables
#' X <- matrix(rnorm(n * p), nrow = n)
#' # Measurement matrix (this is the one we observe)
#' W <- X + matrix(rnorm(n*p, sd = 1), nrow = n, ncol = p)
#' # Coefficient vector
#' beta <- c(seq(from = 0.1, to = 1, length.out = 5), rep(0, p-5))
#' # Response
#' y <- X %*% beta + rnorm(n, sd = 1)
#' # Run the MU Selector
#' gmus1 <- fit_gmus(W, y)
#' # Draw an elbow plot to select delta
#' plot(gmus1)
#'
#' # Now, according to the "elbow rule", choose
#' # the final delta where the curve has an "elbow".
#' # In this case, the elbow is at about delta = 0.08,
#' # so we use this to compute the final estimate:
#' gmus2 <- fit_gmus(W, y, delta = 0.08)
#' # Plot the coefficients
#' plot(gmus2)
#'
#' @export
fit_gmus <- function(W, y, lambda = NULL, delta = NULL,
                     family = c("gaussian", "binomial", "poisson")) {

  family <- match.arg(family)

  if(is.null(lambda)) lambda <- cv.glmnet(W, y, family = family)$lambda.min
  if(is.null(delta)) delta <- seq(from = 0, to = 0.5, by = 0.02)

  n <- dim(W)[1]
  p <- dim(W)[2] + 1
  W <- scale(W)
  scales <- attr(W, "scaled:scale")
  W <- cbind(rep(1,n), W)

  if(family == "gaussian") {
    fit <- sapply(delta, function(delta, W, y, lambda) musalgorithm(W, y, lambda, delta),
                  W, y, lambda)
  } else if(family %in% c("binomial", "poisson")) {
    fit <- sapply(delta, function(delta, W, y, lambda) mus_glm(W, y, lambda, delta), W, y, lambda)
  }


  fit <- list(intercept = fit[1, ],
              beta = fit[2:p, ] / scales,
              family = family,
              delta = delta,
              lambda = lambda,
              num_non_zero = colSums(fit[2:p, , drop = FALSE] > 0)
              )

  class(fit) <- "gmus"
  return(fit)
}



