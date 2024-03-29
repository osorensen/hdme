% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.corrected_lasso.R
\name{plot.corrected_lasso}
\alias{plot.corrected_lasso}
\title{plot.corrected_lasso}
\usage{
\method{plot}{corrected_lasso}(x, type = "nonzero", label = FALSE, ...)
}
\arguments{
\item{x}{Object of class corrected_lasso, returned from calling
corrected_lasso()}

\item{type}{Type of plot. Either "nonzero" or "path". Ignored if
\code{length(x$radii) == 1}, in case of which all coefficient estimates are
plotted at the given regularization parameter.}

\item{label}{Logical specifying whether to add labels to coefficient paths.
Only used when \code{type = "path"}.}

\item{...}{Other arguments to plot (not used)}
}
\description{
Plot the output of corrected_lasso
}
\examples{
# Example with linear regression
n <- 100 # Number of samples
p <- 50 # Number of covariates
# True (latent) variables
X <- matrix(rnorm(n * p), nrow = n)
# Measurement error covariance matrix
# (typically estimated by replicate measurements)
sigmaUU <- diag(x = 0.2, nrow = p, ncol = p)
# Measurement matrix (this is the one we observe)
W <- X + rnorm(n, sd = sqrt(diag(sigmaUU)))
# Coefficient
beta <- c(seq(from = 0.1, to = 1, length.out = 5), rep(0, p-5))
# Response
y <- X \%*\% beta + rnorm(n, sd = 1)
# Run the corrected lasso
fit <- corrected_lasso(W, y, sigmaUU, family = "gaussian")
plot(fit)

}
