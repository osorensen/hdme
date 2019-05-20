#' Generalized Matrix Uncertainty Selector
#' @description Generalized Matrix Uncertainty Selector
#' @param W Design matrix, measured with error. Must be a numeric matrix.
#' @param y Vector of responses.
#' @param lambda Regularization parameter.
#' @param delta Additional regularization parameter, bounding the measurement
#'   error.
#' @param family "gaussian" for linear regression, "binomial" for logistic
#'   regression or "poisson" for Poisson regression. Defaults go "gaussian".
#' @param weights A vector of weights for each row of \code{X}.
#' @return An object of class "gmus".
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
#' fit1 <- gmus(W, y)
#' # Draw an elbow plot to select delta
#' plot(fit1)
#' coef(fit1)
#'
#' # Now, according to the "elbow rule", choose
#' # the final delta where the curve has an "elbow".
#' # In this case, the elbow is at about delta = 0.08,
#' # so we use this to compute the final estimate:
#' fit2 <- gmus(W, y, delta = 0.08)
#' # Plot the coefficients
#' plot(fit2)
#' coef(fit2)
#' coef(fit2, all = TRUE)
#'
#' @export
gmus <- function(W, y, lambda = NULL, delta = NULL,
                     family = "gaussian", weights = NULL) {

  family <- match.arg(family, choices = c("gaussian", "binomial", "poisson"))

  if(!is.null(weights) & length(weights) != nrow(W)) stop("weights vector must be one value per case")

  if(is.null(lambda)) {
    lambda <- glmnet::cv.glmnet(W, y, family = family)$lambda.min
  } else {
    stopifnot(all(lambda >= 0))
  }
  if(is.null(delta)) {
    delta <- seq(from = 0, to = 0.5, by = 0.02)
  } else {
    stopifnot(all(delta >= 0))
  }

  n <- dim(W)[1]
  p <- dim(W)[2] + 1
  W <- scale(W)
  scales <- attr(W, "scaled:scale")
  W <- cbind(rep(1,n), W)

  if(family == "gaussian") {
    fit <- sapply(delta, function(delta) musalgorithm(W, y, lambda, delta, weights))
  } else if(family %in% c("binomial", "poisson")) {
    fit <- sapply(delta, function(delta) mus_glm(W, y, lambda, delta, family, weights))
  }


  fit <- list(intercept = fit[1, ],
              beta = matrix(fit[2:p, ] / scales, nrow = p - 1),
              family = family,
              delta = delta,
              lambda = lambda,
              num_non_zero = colSums(abs(fit[2:p, , drop = FALSE]) > 1e-10)
              )

  class(fit) <- "gmus"
  return(fit)
}



