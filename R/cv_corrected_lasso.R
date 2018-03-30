#' Cross-validated Corrected lasso
#'
#' @details Corrected version of the lasso for the case of linear regression,
#'   estimated using cross-validation. The method does require an estimate of
#'   the measurement error covariance matrix.
#' @param W Design matrix, measured with error.
#' @param y Vector of the continuous response value.
#' @param sigmaUU Covariance matrix of the measurement error.
#' @param n_folds Number of folds to use in cross-validation. Default is 100.
#' @param family Only "gaussian" is implemented at the moment.
#' @param radii Optional vector containing the set of radii of the l1-ball onto
#'   which the solution is projected.
#' @param no_radii Length of vector radii, i.e., the number of regularization
#'   parameters to fit the corrected lasso for.
#' @param alpha Optional step size of the projected gradient descent algorithm.
#'   Default is 0.1.
#' @param maxits Optional maximum number of iterations of the project gradient
#'   descent algorithm for each radius. Default is 5000.
#' @return A list
#' @references \insertRef{loh2012}{hdme}
#'
#' \insertRef{sorensen2015}{hdme}
#'
#' @examples
#' # Gaussian
#' set.seed(100)
#' n <- 100; p <- 50 # Problem dimensions
#' # True (latent) variables
#' X <- matrix(rnorm(n * p), nrow = n)
#' # Measurement error covariance matrix
#' # (typically estimated by replicate measurements)
#' sigmaUU <- diag(x = 0.2, nrow = p, ncol = p)
#' # Measurement matrix (this is the one we observe)
#' W <- X + rnorm(n, sd = diag(sigmaUU))
#' # Coefficient
#' beta <- c(seq(from = 0.1, to = 1, length.out = 5), rep(0, p-5))
#' # Response
#' y <- X %*% beta + rnorm(n, sd = 1)
#' # Run the corrected lasso
#' cvfit <- cv_corrected_lasso(W, y, sigmaUU, no_radii = 5, n_folds = 3)
#' plot(cvfit)
#' # Run the standard lasso using the radius found by cross-validation
#' fit <- fit_corrected_lasso(W, y, sigmaUU, family = "gaussian",
#' radii = cvfit$radius_min)
#' @export
cv_corrected_lasso <- function(W, y, sigmaUU, n_folds = 10, family = "gaussian",
                 radii = NULL, no_radii = 100, alpha = 0.1, maxits = 5000){
  N = nrow(W)
  y <- drop(y)
  fold_id = sample(rep(seq(n_folds), length = N))
  outlist = as.list(seq(n_folds))


  if(is.null(radii)) radii <- set_radius(W, y, no_radii)
  loss <- matrix(nrow = no_radii, ncol = n_folds)

  for(i in seq(n_folds)) {
    test = (fold_id == i)
    outlist[[i]] <- fit_corrected_lasso(W = W[!test, , drop = FALSE], y = y[!test], sigmaUU = sigmaUU, radii = radii)
    loss[, i] <- gauss_loss(W = W[test, , drop = FALSE], y = y[test], sigmaUU = sigmaUU, beta = outlist[[i]]$betaCorr)
  }

  cv <- data.frame(
    radii = radii,
    mean_loss = rowMeans(loss),
    sd_loss = apply(loss, 1, sd),
    upper_1se = rowMeans(loss) + apply(loss, 1, sd) / sqrt(n_folds),
    lower_1se = rowMeans(loss) - apply(loss, 1, sd) / sqrt(n_folds)
  )

  ind1 <- min(which(cv$mean_loss == min(cv$mean_loss)))
  radius_min <- cv$radii[ind1]
  loss_min <- cv$mean_loss[ind1]

  ind2 <- min(which(min(cv$mean_loss) > cv$lower_1se))
  radius_1se <- cv$radii[ind2]
  loss_1se <- cv$mean_loss[ind2]

  fit <- list(cv = cv,
              radius_min = radius_min,
              loss_min = loss_min,
              radius_1se = radius_1se,
              loss_1se = loss_1se
              )

  class(fit) <- c("cv_corrected_lasso")

  return(fit)

}
