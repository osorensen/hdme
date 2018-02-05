correctedLassoGaussian <- function(W, y,sigmaUU, radius, noRadii, alpha, maxits){

  # Mean-subtract the columns of W
  W <- scale(W, scale = FALSE)

  # Mean-subtract the response (we do not care about the intercept)
  y <- y - mean(y)


  if( is.null(radius) ){
    noRadii <- ifelse(is.null(noRadii), 20, noRadii)
    # First run the naive Lasso
    lassoFit <- glmnet::cv.glmnet(W, y)
    betaNaive <- glmnet::coef.cv.glmnet(lassoFit, s = "lambda.min")

    # Use the estimated vector to find the upper radius for cross-validation
    R <- 2 * sum( abs( betaNaive ) )
    # Set the cross-validation range
    radius <- seq(from = 1e-3 * R, to = R, length.out = noRadii)
  } else {
    noRadii <- length(radius)
  }

  n <- dim(W)[1]
  p <- dim(W)[2]

  # Initiate the coefficient vector
  betaCorr <- matrix(nrow = p, ncol = noRadii + 1)
  betaCorr[, 1] <- rep(0, p)
  Q <- (1/n) * t(W) %*% W - sigmaUU
  b <- (1/n) * t(W) %*% y

  for(r in 2 : (noRadii + 1)) {
    # Compute the estimate
    betaCorr[, r] <- doProjGrad(Q, b, maxits, alpha, radius[r - 1], betaCorr[, r-1])
  }


  value <- list(betaCorr = betaCorr[, -1, drop = FALSE],
                radius = radius)

  return(value)
}
