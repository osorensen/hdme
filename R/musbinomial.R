#' Generalized matrix uncertainty selector.
#'
#' @description Generalized Matrix Uncertainty Selector, an extension of the MU Selector to generalized linear models. The model was proposed by Sorensen et al. 2014.
#' @param W Design matrix, measured with error.
#' @param y Vector of the binomial response value.
#' @param lambda Regularization parameter due to model error.
#' @param delta Regularization parameter due to measurement error.
#' @param family Character string describing the family to be used. Only "binomial" is available at the moment, and is provided by default.
#' @return Intercept and coefficients at the values of lambda and delta specified.
#' @references Emmanuel Candes and Terence Tao. 2007. "The Dantzig Selector: Statistical Estimation When p Is Much Larger Than n." The Annals of Statistics 35 (6) https://projecteuclid.org/euclid.aos/1201012958
#' @references Gareth M. James and Peter Radchenko. 2009. "A Generalized Dantzig Selector with Shrinkage Tuning." Biometrika 96 (2) https://biomet.oxfordjournals.org/content/96/2/323.short
#' @references Mathieu Rosenbaum and Alexandre B. Tsybakov. 2010. "Sparse Recovery Under Matrix Uncertainty." The Annals of Statistics 38 (5) https://projecteuclid.org/euclid.aos/1278861455
#' @references Oystein Sorensen, Arnoldo Frigessi, and Magne Thoresen. 2014. "Covariate Selection in High-Dimensional Generalized Linear Models with Measurement Error." https://arxiv.org/abs/1407.1070
#' @examples
#' set.seed(1)
#' # Problem dimensions
#' n <- 500; p <- 50
#'
#' # True (latent) variables
#' X <- matrix(rnorm(n * p), nrow = n)
#'
#' # Measurement matrix (this is the one we observe)
#' W <- X + matrix(rnorm(n*p, sd = 1), nrow = n, ncol = p)
#'
#' # Binary response
#' beta <- c(seq(from = 0.1, to = 1, length.out = 5), rep(0, p-5))
#' logit <- function(x) (1+exp(-x))^(-1)
#' y <- rbinom(n, 1, logit(X %*% beta))
#'
#' # Run the GMU Selector at lambda = 0.1 and delta = 0.1
#' fit <- GMUSelector(W, y, 0.1, 0.1)
#'
#' # Estimate the regularization parameters:
#' # First compute the optimal cross-validated lambda value for the standard lasso
#' library(glmnet)
#' lassoFit <- cv.glmnet(W, y, family = "binomial")
#' lambdaMin <- lassoFit$lambda.min
#'
#' # Next, use this lambda, and draw an elbow plot for delta
#' delta <- seq(from = 0, to = 0.5, by = 0.02)
#' support <- sapply(delta,
#'                  function(x)
#'                    sum(GMUSelector(W, y, lambdaMin, delta = x)$coefficients != 0)
#'                    )
#'
#' # Create an elbow plot
#' plot(delta, support, type = "l",
#'  xlab = expression(delta),
#'  ylab = "Nonzero coefficients",
#'  main = "Elbow plot",
#'  ylim = c(0, max(support)))
#'
#' # Now, according to the "elbow rule", choose the final delta where the
#' # curve has an "elbow". In this case, the elbow is at about delta = 0.12,
#' # so we use this to compute the final estimate:
#' GMUSelectorFit <- GMUSelector(W, y, lambdaMin, 0.12)
#'
#' # Compare the GMUSelector fit to the Lasso fit
#' plot(1:p, beta, xlab = "Coefficient no.", ylab = "Coefficient value", ylim = c(-0.2, 1.1))
#' points(1:p, coefficients(lassoFit)[-1], col = "red")
#' points(1:p, GMUSelectorFit$coefficients, col = "blue")
#' legend(35, 0.9,
#'  legend = c("True value", "Standard lasso", "GMU Selector"),
#'  col = c("black", "red", "blue"), pch = 20)
#'  @import stats
musbinomial <- function(W, y, lambda, delta){
  W <- scale(W)
  scales <- attr(W, "scaled:scale")

  mu <- logit
  dmu <- dlogit

  n <- dim(W)[1]
  p <- dim(W)[2]

  bOld <- rnorm(p)/p
  bNew <- rnorm(p)/p
  IRLSeps <- 1e-7
  maxit <- 100
  count <- 1
  Diff1 <- 1
  Diff2 <- 1

  while(Diff1 > IRLSeps & Diff2 > IRLSeps & count < maxit){
      bOlder <- bOld
      bOld <- bNew
      V <- dmu(W%*%bOld)
      z <- W%*%bOld + (y - mu(W%*%bOld))/dmu(W%*%bOld)
      Wtilde <- c(sqrt(V)) * W
      ztilde <- c(sqrt(V)) * c(z)
      value <- musalgorithm(Wtilde, ztilde, lambda, delta * sqrt(sum((V)^2)) / sqrt(n))
      betaNew <- value$coefficients
      count <- count+1
      Diff1 <- sum(abs(bNew - bOld))
      Diff2 <- sum(abs(bNew - bOlder))
  }
  if(count >= maxit) print(paste("Did not converge"))
  return(value)
}
