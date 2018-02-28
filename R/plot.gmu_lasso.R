#' @title Plot the estimates returned by fit_gmu_lasso
#' @description Plot the number of nonzero coefficients along a range of delta
#'   values if delta has length larger than 1, or the estimated coefficients of
#'   delta has length 1.
#' @param x An object of class gmu_lasso
#' @param ... Other arguments to plot (not used).
#' @import ggplot2
#' @examples
#' n <- 200
#' p <- 50
#' s <- 10
#' beta <- c(rep(1,s),rep(0,p-s))
#' sdU <- 0.2
#'
#' X <- matrix(rnorm(n*p),nrow = n,ncol = p)
#' W <- X + sdU * matrix(rnorm(n * p), nrow = n, ncol = p)
#'
#' y <- rbinom(n, 1, (1 + exp(-X%*%beta))**(-1))
#' gmu_lasso <- fit_gmu_lasso(W, y)
#'
#' plot(gmu_lasso)
#'
#' @export
plot.gmu_lasso <- function(x, ...) {
  plot.gmus(x)
}
