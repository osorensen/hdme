#' @title plot.corrected_lasso
#' @description Plot the output of corrected_lasso
#' @param x Object of class corrected_lasso, returned from calling
#'   corrected_lasso()
#' @param type Type of plot. Either "nonzero" or "path". Ignored if
#'   \code{length(x$radii) == 1}, in case of which all coefficient estimates are
#'   plotted at the given regularization parameter.
#' @param label Logical specifying whether to add labels to coefficient paths.
#'   Only used when \code{type = "path"}.
#' @param ... Other arguments to plot (not used)
#' @examples
#' # Example with linear regression
#' n <- 100 # Number of samples
#' p <- 50 # Number of covariates
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
#' plot(fit)
#'
#' @export
plot.corrected_lasso <- function(x, type = "nonzero", label = FALSE, ...) {

  if(length(x$radii) == 1){
    message("Only one regularization parameter. Plotting all coefficients.\n")
    df <- data.frame(
      coefficient = 1:length(x$betaCorr),
      estimate = x$betaCorr
    )
    ggplot2::ggplot(df, ggplot2::aes(x = .data$coefficient, y = .data$estimate)) +
      ggplot2::geom_point()
  } else if(type == "nonzero") {
    df <- data.frame(radius = x$radii, nonZero = colSums(abs(x$betaCorr) > 0))
    ggplot2::ggplot(df, ggplot2::aes(x = .data$radius, y = .data$nonZero)) +
      ggplot2::geom_line() +
      ggplot2::labs(x = "radius", y = "Nonzero coefficients", title = "Number of nonzero coefficients")
  } else if (type == "path") {
    df <- data.frame(radius = rep(x$radii, nrow(x$betaCorr)),
                     coefficient_id = as.factor(rep(seq_along(1:nrow(x$betaCorr)), each = length(x$radii))),
                     beta_corr = as.vector(t(x$betaCorr)))

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$radius, y = .data$beta_corr, color = .data$coefficient_id)) +
      ggplot2::geom_path() +
      ggplot2::labs(x = "radius", y = "Coefficient estimate", title = "Coefficient paths") +
      ggplot2::theme(legend.position="none")

    if(label){
      lab_df <- df[df$radius == max(x$radii), ]
      p +
        ggplot2::geom_label(data = lab_df, ggplot2::aes(label = .data$coefficient_id))
    } else {
      p
    }

  } else {
    stop("type argument must have value 'nonzero' or 'path'")
  }
}
