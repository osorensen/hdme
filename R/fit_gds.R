#' Generalized Dantzig Selector
#' @description Generalized Dantzig Selector for generalized
#' @import glmnet
#' @param W Design matrix, measured with error.
#' @param y Vector of the continuous response value.
#' @param lambda Regularization parameter due to model error.
#' @param delta Regularization parameter due to measurement error.
#' @return Intercept and coefficients at the values of lambda and delta specified.
#' @references Emmanuel Candes and Terence Tao. 2007. "The Dantzig Selector: Statistical Estimation When p Is Much Larger Than n." The Annals of Statistics 35 (6) https://projecteuclid.org/euclid.aos/1201012958
#' @references Mathieu Rosenbaum and Alexandre B. Tsybakov. 2010. "Sparse Recovery Under Matrix Uncertainty." The Annals of Statistics 38 (5) https://projecteuclid.org/euclid.aos/1278861455
#' @examples
#' set.seed(1)
#' n <- 100; p <- 50 # Problem dimensions
#' X <- matrix(rnorm(n * p), nrow = n) # True (latent) variables
#' W <- X + matrix(rnorm(n*p, sd = 1), nrow = n, ncol = p) # Measurement matrix (this is the one we observe)
#' beta <- c(seq(from = 0.1, to = 1, length.out = 5), rep(0, p-5))
#' y <- X %*% beta + rnorm(n, sd = 1) # Response
#' fit <- muselector(W, y) # Run the MU Selector
#' plot(fit) # Draw an elbow plot to select delta
#'
#' # Now, according to the "elbow rule", choose the final delta where the curve has an "elbow".
#' # In this case, the elbow is at about delta = 0.12, so we use this to compute the final estimate:
#' fit <- muselector(W, y, delta = 0.12)
#' plot(fit) # Plot the coefficients
#'
#' @export
fit_gds <- function(X, y, lambda = NULL, delta = 0, family = c("gaussian", "binomial")) {
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
              nonZero = colSums(fit[2:p, , drop = FALSE] > 0)
  )
  class(fit) <- c("muselector", class(fit))
  return(fit)
}



