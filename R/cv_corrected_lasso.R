#' Cross-validated Corrected lasso
#'
#' @details Corrected version of the lasso for the case of linear regression, estimated using cross-validation. The method does require an estimate of the measurement error covariance matrix.
#' @param W Design matrix, measured with error.
#' @param y Vector of the continuous response value.
#' @param sigmaUU Covariance matrix of the measurement error.
#' @param nfolds Number of folds to use in cross-validation. Default is 100.
#' @param radius Optional vector containing the set of radii of the l1-ball onto which the solution is projected.
#' @param alpha Optional step size of the projected gradient descent algorithm. Default is 0.1.
#' @param maxits Optional maxium number of iterations of the project gradient descent algorithm for each radius. Default is 5000.
#' @return An object with S3 class "cv.correctedLasso", "list"
#' @references Po-Ling Loh, and Martin J. Wainwright. 2012. "High-Dimensional Regression with Noisy and Missing Data: Provable Guarantees with Nonconvexity." The Annals of Statistics 40 (3) https://projecteuclid.org/euclid.aos/1346850068
#' @references Oystein Sorensen, Arnoldo Frigessi, and Magne Thoresen. 2015. "Measurement Error in Lasso: Impact and Likelihood Bias Correction." Statistica Sinica 25 (2).
#' @import dplyr
#' @examples
#' # Gaussian
#' set.seed(100)
#' n <- 100; p <- 50 # Problem dimensions
#' X <- matrix(rnorm(n * p), nrow = n)  # True (latent) variables
#' sigmaUU <- diag(x = 0.2, nrow = p, ncol = p) # Measurement error covariance matrix (typically estimated by replicate measurements)
#' W <- X + rnorm(n, sd = diag(sigmaUU)) # Measurement matrix (this is the one we observe)
#' beta <- c(seq(from = 0.1, to = 1, length.out = 5), rep(0, p-5)) # Coefficient
#' y <- X %*% beta + rnorm(n, sd = 1) # Response
#' cvfit <- cv.corrected_lasso(W, y, sigmaUU) # Run the corrected lasso
#' plot(cvfit)
#' # Run the standard lasso using the radius found by cross-validation
#' fit <- corrected_lasso(W, y, sigmaUU, family = "gaussian", radius = cvfit$radius.min)
#' @export
cv_corrected_lasso <- function(W, y, sigmaUU, nfolds = 10,
                 radius = NULL, noRadii = 100, alpha = 0.1, maxits = 5000, standardize = FALSE){
  N = nrow(W)
  y <- drop(y)
  foldid = sample(rep(seq(nfolds), length = N))
  outlist = as.list(seq(nfolds))


  if(is.null(radius)) radius <- setRadius(W, y, noRadii)
  loss <- matrix(nrow = noRadii, ncol = nfolds)

  for(i in seq(nfolds)) {
    test = (foldid == i)
    outlist[[i]] <- correctedLasso(W = W[!test, , drop = FALSE], y = y[!test], sigmaUU = sigmaUU, radius = radius)
    loss[, i] <- gaussLoss(W = W[test, , drop = FALSE], y = y[test], sigmaUU = sigmaUU, beta = outlist[[i]]$betaCorr)
  }

  cv <- data.frame(
    radius = radius,
    meanLoss = rowMeans(loss),
    sdLoss = apply(loss, 1, sd),
    upper1se = rowMeans(loss) + apply(loss, 1, sd) / sqrt(nfolds),
    lower1se = rowMeans(loss) - apply(loss, 1, sd) / sqrt(nfolds)
  )

  ind1 <- min(which(cv$meanLoss == min(cv$meanLoss)))
  radius.min <- cv$radius[ind1]
  loss.min <- cv$meanLoss[ind1]

  ind2 <- min(which(min(cv$meanLoss) > cv$lower1se))
  radius.1se <- cv$radius[ind2]
  loss.1se <- cv$meanLoss[ind2]

  fit <- list(cv = cv,
              radius.min = radius.min,
              loss.min = loss.min,
              radius.1se = radius.1se,
              loss.1se = loss.1se
              )

  class(fit) <- c("cv_correctedLasso", class(fit))

  return(fit)

}
