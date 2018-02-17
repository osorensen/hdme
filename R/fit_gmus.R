#' Generalized Matrix Uncertainty Selector
#' @description Generalized Matrix Uncertainty Selector
#' @import glmnet
#' @param W Design matrix, measured with error. Must be a numeric matrix.
#' @param y Vector of responses.
#' @param lambda Regularization parameter.
#' @param delta Additional regularization parameter, bounding the measurement error.
#' @param family "gaussian" for linear regression and "binomial" for logistic regression.
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
#' gmus1 <- fit_gmus(W, y) # Run the MU Selector
#' plot(fit) # Draw an elbow plot to select delta
#'
#' # Now, according to the "elbow rule", choose the final delta where the curve has an "elbow".
#' # In this case, the elbow is at about delta = 0.08, so we use this to compute the final estimate:
#' gmus2 <- fit_gmus(W, y, delta = 0.08)
#' plot(fit) # Plot the coefficients
#'
#' @export
fit_gmus <- function(W, y, lambda = NULL, delta = NULL, family = c("gaussian", "binomial")) {
  family <- match.arg(family)

  if(is.null(lambda)) lambda <- cv.glmnet(W, y, family = family)$lambda.min
  if(is.null(delta)) delta <- seq(from = 0, to = 0.5, by = 0.02)

  n <- dim(W)[1]
  p <- dim(W)[2] + 1
  W <- scale(W)
  scales <- attr(W, "scaled:scale")
  W <- cbind(rep(1,n), W)

  fit <- switch(family,
                "gaussian" = sapply(delta, function(delta, W, y, lambda) musalgorithm(W, y, lambda, delta), W, y, lambda),
                "binomial" = sapply(delta, function(delta, W, y, lambda) musbinomial(W, y, lambda, delta), W, y, lambda))



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



