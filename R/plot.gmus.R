#' Plot the estimates returned by fit_gmus and fit_mus
#' @description Plot the number of nonzero coefficients along a range of delta
#'   values if delta has length larger than 1, or the estimated coefficients of
#'   delta has length 1.
#' @param x An object of class gmus
#' @import ggplot2
#' @examples
#' # Example with linear regression
#' set.seed(1)
#' n <- 1000 # Number of samples
#' p <- 200 # Number of covariates
#' X <- matrix(rnorm(n * p), nrow = n) # True (latent) variables
#' W <- X + matrix(rnorm(n*p, sd = 0.4), nrow = n, ncol = p) # Measurement matrix (this is the one we observe)
#' beta <- c(seq(from = 0.1, to = 1, length.out = 5), rep(0, p-5)) # Coefficient vector
#' y <- X %*% beta + rnorm(n, sd = 1) # Response
#' mus1 <- fit_mus(W, y) # Run the MU Selector
#' plot(mus1) # Draw an elbow plot to select delta
#'
#' # Now, according to the "elbow rule", choose the final delta where the curve has an "elbow".
#' # In this case, the elbow is at about delta = 0.08, so we use this to compute the final estimate:
#' mus2 <- fit_mus(W, y, delta = 0.08)
#' plot(mus2) # Plot the coefficients
#'
#' @export
plot.gmus <- function(x) {
  if(length(x$delta) > 1) {
    df <- data.frame(delta = x$delta, nonzero = x$num_non_zero)
    ggplot(df, aes(delta, nonzero)) +
      geom_line() +
      labs(x = "delta", y = "Nonzero coefficients", title = "Elbow plot")
  } else {
    df <- data.frame(index = seq_along(x$beta), beta = x$beta)

    ggplot(df, aes(index, beta)) +
      geom_point() +
      labs(x = "Coefficient number", y = "Coefficient value", title = "Estimated coefficients")
  }

}
