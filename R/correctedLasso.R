#' Corrected lasso
#'
#' @details Corrected version of the lasso for generalized linear models. The method does require an estimate of the measurement error covariance matrix.
#' @param W Design matrix, measured with error.
#' @param y Vector of the continuous response value.
#' @param sigmaUU Covariance matrix of the measurement error.
#' @param family Response type.
#' @param radii Optional vector containing the set of radii of the l1-ball onto which the solution is projected. If not provided, the algorithm will select an evenly spaced vector of 20 radii.
#' @param alpha Optional step size of the projected gradient descent algorithm. Default is 0.1.
#' @param maxits Optional maxium number of iterations of the project gradient descent algorithm for each radius. Default is 5000.
#' @return Returns a list containing a matrix whose columns represent the corrected beta estimates at each radius, as well as the vector of radii used.
#' @references Po-Ling Loh, and Martin J. Wainwright. 2012. "High-Dimensional Regression with Noisy and Missing Data: Provable Guarantees with Nonconvexity." The Annals of Statistics 40 (3) https://projecteuclid.org/euclid.aos/1346850068
#' @references Oystein Sorensen, Arnoldo Frigessi, and Magne Thoresen. 2015. "Measurement Error in Lasso: Impact and Likelihood Bias Correction." Statistica Sinica 25 (2).
#' @examples
#' # Gaussian
#' n <- 100; p <- 50 # Problem dimensions
#' X <- matrix(rnorm(n * p), nrow = n)  # True (latent) variables
#' sigmaUU <- diag(x = 0.2, nrow = p, ncol = p) # Measurement error covariance matrix (typically estimated by replicate measurements)
#' W <- X + rnorm(n, sd = diag(sigmaUU)) # Measurement matrix (this is the one we observe)
#' beta <- c(seq(from = 0.1, to = 1, length.out = 5), rep(0, p-5)) # Coefficient
#' y <- X %*% beta + rnorm(n, sd = 1) # Response
#' fit <- correctedLasso(W, y, sigmaUU, family = "gaussian") # Run the corrected lasso
#' plot(fit)
#'
#' # Binomial
#' n <- 1000; p <- 50
#' X <- matrix(rnorm(n * p), nrow = n) # True (latent) variables
#' sigmaUU <- diag(x = 0.2, nrow = p, ncol = p) # Measurement error covariance matrix
#' W <- X + rnorm(n, sd = diag(sigmaUU)) # Measurement matrix (this is the one we observe)
#' logit <- function(x) (1+exp(-x))^(-1)
#' y <- rbinom(n, size = 1, prob = logit(X %*% c(rep(5, 5), rep(0, p-5)))) # Response
#' fit <- correctedLasso(W, y, sigmaUU, family = "binomial")
#' plot(fit)
#'
#' @export
correctedLasso <- function(W, y, sigmaUU, family = c("gaussian", "binomial", "poisson"),
                 radius = NULL, noRadii = 20, alpha = 0.1, maxits = 5000, standardize = FALSE){
  family <- match.arg(family)

  fit <- switch(family,
             "gaussian" = correctedLassoGaussian(W = W, y = y, sigmaUU = sigmaUU, radius = radius, noRadii = noRadii, alpha = alpha, maxits = maxits, standardize = standardize),
             "binomial" = correctedLassoBinomial(W = W, y = y, sigmaUU = sigmaUU, radius = radius, noRadii = noRadii, alpha = alpha, maxits = maxits, standardize = standardize)
             )

  class(fit) <- c("correctedLasso", class(fit))

  return(fit)

}
