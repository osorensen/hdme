#' Matrix Uncertainty Selector (MU Selector)
#' @description Matrix Uncertainty Selector (MU Selector) for linear regression, a modified version of the Dantzig Selector for the case of covariate measurement error. The method was first proposed by Rosenbaum and Tsybakov (2010). The MU Selector does not require an estimate of the measurement error covariance matrix, but instead takes measurement error into account through an additional regularization parameter, delta.
#' @param W Design matrix, measured with error.
#' @param y Vector of the continuous response value.
#' @param lambda Regularization parameter due to model error.
#' @param delta Regularization parameter due to measurement error.
#' @return Intercept and coefficients at the values of lambda and delta specified.
#' @references Emmanuel Candes and Terence Tao. 2007. "The Dantzig Selector: Statistical Estimation When p Is Much Larger Than n." The Annals of Statistics 35 (6) https://projecteuclid.org/euclid.aos/1201012958
#' @references Mathieu Rosenbaum and Alexandre B. Tsybakov. 2010. "Sparse Recovery Under Matrix Uncertainty." The Annals of Statistics 38 (5) https://projecteuclid.org/euclid.aos/1278861455
#' @examples
#' set.seed(1)
#' # Problem dimensions
#' n <- 100; p <- 50
#'
#' # True (latent) variables
#' X <- matrix(rnorm(n * p), nrow = n)
#'
#' # Measurement matrix (this is the one we observe)
#' W <- X + matrix(rnorm(n*p, sd = 1), nrow = n, ncol = p)
#'
#' # Response
#' beta <- c(seq(from = 0.1, to = 1, length.out = 5), rep(0, p-5))
#' y <- X %*% beta + rnorm(n, sd = 1)
#'
#' # Run the MU Selector at lambda = 0.1 and delta = 0.1
#' fit <- MUSelector(W, y, 0.1, 0.1)
#'
#' # Estimate the regularization parameters:
#' # First compute the optimal cross-validated lambda value for the standard lasso
#' library(glmnet)
#' lassoFit <- cv.glmnet(W, y)
#' lambdaMin <- lassoFit$lambda.min
#'
#' # Next, use this lambda, and draw an elbow plot for delta
#' delta <- seq(from = 0, to = 0.5, by = 0.02)
#' support <- sapply(delta, function(x) sum(MUSelector(W, y, lambdaMin, delta = x)$coefficients != 0))
#'
#' # Create an elbow plot
#' plot(delta, support, type = "l",
#'  xlab = expression(delta),
#'  ylab = "Nonzero coefficients",
#'  main = "Elbow plot",
#'  ylim = c(0, max(support)))
#'
#' # Now, according to the "elbow rule", choose the final delta where the curve has an "elbow".
#' # In this case, the elbow is at about delta = 0.12, so we use this to compute the final estimate:
#' MUSelectorFit <- MUSelector(W, y, lambdaMin, 0.12)
#'
#' # Compare the MUSelector fit to the Lasso fit
#' plot(1:p, beta, xlab = "Coefficient no.", ylab = "Coefficient value", ylim = c(-0.2, 1.1))
#' points(1:p, coefficients(lassoFit)[-1], col = "red")
#' points(1:p, MUSelectorFit$coefficients, col = "blue")
#' legend(35, 0.9,
#'  legend = c("True value", "Standard lasso", "MU Selector"),
#'  col = c("black", "red", "blue"), pch = 20)
#'
#' @export
muselector <- function(W, y, lambda, delta, family = c("gaussian", "binomial")) {
  family <- match.arg(family)
  n <- dim(W)[1]
  p <- dim(W)[2] + 1
  W <- scale(W)
  scales <- attr(W, "scaled:scale")
  W <- cbind(rep(1,n), W)

  fit <- switch(family,
                "gaussian" = musalgorithm(W, y, lambda, delta),
                "binomial" = musbinomial(W, y, lambda, delta))

  fit <- list(intercept = value[1],
              beta = value[2:p] / scales,
              family = family
              )
  class(fit) <- "muselector"
  return(fit)
}



