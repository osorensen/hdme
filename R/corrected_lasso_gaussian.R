corrected_lasso_gaussian <- function(W, y,sigmaUU, radii, no_radii, alpha, maxits){

  # Mean-subtract the columns of W
  W <- scale(W, scale = FALSE)

  # Mean-subtract the response (we do not care about the intercept)
  y <- y - mean(y)


  if( is.null(radii) ){
    no_radii <- ifelse(is.null(no_radii), 20, no_radii)
    # First run the naive Lasso
    lassoFit <- glmnet::cv.glmnet(W, y)
    betaNaive <- glmnet::coef.cv.glmnet(lassoFit, s = "lambda.min")

    # Use the estimated vector to find the upper radii for cross-validation
    R <- 2 * sum( abs( betaNaive ) )

    # Set the cross-validation range
    radii <- seq(from = 1e-3 * R, to = R, length.out = no_radii)
  } else {
    no_radii <- length(radii)
  }

  n <- dim(W)[1]
  p <- dim(W)[2]

  # Initiate the coefficient vector
  betaCorr <- matrix(nrow = p, ncol = no_radii + 1)
  betaCorr[, 1] <- rep(0, p)
  Q <- (1/n) * t(W) %*% W - sigmaUU
  b <- (1/n) * t(W) %*% y

  for(r in 2 : (no_radii + 1)) {
    # Compute the estimate
    betaCorr[, r] <- project_gradient(Q, b, maxits, alpha, radii[r - 1], betaCorr[, r-1])
  }


  value <- list(betaCorr = betaCorr[, -1, drop = FALSE],
                radii = radii)

  return(value)
}
