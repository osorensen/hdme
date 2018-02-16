#' @title plot.corrected_lasso
#' @param x Object of class corrected_lasso, returned from calling fit_corrected_lasso()
#' @param type Type of plot. Either "nonzero" or "path".
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
#' @import ggplot2
#' @export
plot.corrected_lasso <- function(x, type = "nonzero") {

  if(type == "nonzero") {
    df <- data.frame(radius = x$radii, nonZero = colSums(abs(x$betaCorr) > 0))
    ggplot(df, aes(radius, nonZero)) +
      geom_line() +
      labs(x = "radius", y = "Nonzero coefficients", title = "Number of nonzero coefficients")
  } else if (type == "path") {
    df <- data.frame(radius = rep(x$radii, nrow(x$betaCorr)),
                     coefficient_id = as.factor(rep(seq_along(1:nrow(x$betaCorr)), each = length(x$radii))),
                     beta_corr = as.vector(t(x$betaCorr)))

    ggplot(df, aes(x = radius, y = beta_corr, color = coefficient_id)) +
      geom_path() +
      labs(x = "radius", y = "Coefficient estimate", title = "Coefficient paths") +
      theme(legend.position="none")
  } else {
    stop("type argument must have value 'nonzero' or 'path'")
  }
}
