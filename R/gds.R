#' Generalized Dantzig Selector
#' @description Generalized Dantzig Selector
#' @param X Design matrix.
#' @param y Vector of the continuous response value.
#' @param lambda Regularization parameter. Only a single value is supported.
#' @param family Use "gaussian" for linear regression and "binomial" for logistic regression.
#' @return Intercept and coefficients at the values of lambda specified.
#' @references \insertRef{candes2007}{hdme}
#'
#' \insertRef{james2009}{hdme}
#'
#' @examples
#' # Example with logistic regression
#' n <- 1000  # Number of samples
#' p <- 10 # Number of covariates
#' X <- matrix(rnorm(n * p), nrow = n) # True (latent) variables # Design matrix
#' beta <- c(seq(from = 0.1, to = 1, length.out = 5), rep(0, p-5)) # True regression coefficients
#' y <- rbinom(n, 1, (1 + exp(-X %*% beta))^(-1)) # Binomially distributed response
#' fit <- gds(X, y, family = "binomial")
#' print(fit)
#' plot(fit)
#' coef(fit)
#'
#' # Try with more penalization
#' fit <- gds(X, y, family = "binomial", lambda = 0.1)
#' coef(fit)
#' coef(fit, all = TRUE)
#' @export
gds <- function(X, y, lambda = NULL, family = "gaussian") {

  if(!is.null(lambda) & length(lambda) != 1) stop("lambda must be a single value")
  stopifnot(lambda >= 0)
  fit <- gmus(X, y, lambda = lambda, delta = 0, family = family)

  # In the Dantzig selector case, delta is not of interest
  fit$delta <- NULL

  class(fit) <- "gds"
  return(fit)
}



