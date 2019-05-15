#' Generalized Dantzig Selector
#' @description Generalized Dantzig Selector
#' @param X Design matrix.
#' @param y Vector of the continuous response value.
#' @param lambda Regularization parameter. Only a single value is supported.
#' @param family Use "gaussian" for linear regression, "binomial" for logistic regression and "poisson" for Poisson regression.
#' @param weights A vector of weights for each row of \code{X}.
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
#'
#'
#' # Case weighting
#' # Assume we wish to put more emphasis on predicting the positive cases correctly
#' # In this case we give the 1s three times the weight of the zeros.
#' weights <- (y == 0) * 1 + (y == 1) * 3
#' fit_w <- gds(X, y, family = "binomial", weights = weights, lambda = 0.1)
#'
#' # Next we test this on a new dataset, generated with the same parameters
#' X_new <- matrix(rnorm(n * p), nrow = n)
#' y_new <- rbinom(n, 1, (1 + exp(-X_new %*% beta))^(-1))
#' # We use a 50 % threshold as classification rule
#' # Unweighted classifcation
#' classification <- ((1 + exp(- fit$intercept - X_new %*% fit$beta))^(-1) > 0.5) * 1
#' # Weighted classification
#' classification_w <- ((1 + exp(- fit_w$intercept - X_new %*% fit_w$beta))^(-1) > 0.5) * 1
#'
#' # As expected, the weighted classification predicts many more 1s than 0s, since
#' # these are heavily up-weighted
#' table(classification, classification_w)
#'
#' # Here we compare the performance of the weighted and unweighted models.
#' # The weighted model gets most of the 1s right, while the unweighted model
#' # gets the highest overall performance.
#' table(classification, y_new)
#' table(classification_w, y_new)
#'
#' @export
gds <- function(X, y, lambda = NULL, family = "gaussian", weights = NULL) {

  if(!is.null(lambda) & length(lambda) != 1) stop("lambda must be a single value")

  stopifnot(lambda >= 0)
  fit <- gmus(X, y, lambda = lambda, delta = 0, family = family, weights = weights)

  # In the Dantzig selector case, delta is not of interest
  fit$delta <- NULL

  class(fit) <- "gds"
  return(fit)
}



