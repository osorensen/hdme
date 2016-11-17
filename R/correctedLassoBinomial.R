logit <- function(x) (1+exp(-x))^(-1)
dlogit <- function(x) exp(-x)*(1+exp(-x))^(-2)

correctedLassoBinomial <- function(W, y, sigmaUU, radius, noRadii, alpha, maxits, standardize, tol = 1e-10){
  if( is.null(radius) ){
    # First run the naive Lasso
    lassoFit <- glmnet::cv.glmnet(W, y, family = "binomial")
    betaNaive <- glmnet::coef.cv.glmnet(lassoFit, s = "lambda.min")

    noRadii <- 20
    # Use the estimated vector to find the upper radius for cross-validation
    R <- sum( abs( betaNaive ) )
    # Set the cross-validation range
    radius <- seq(from = 1e-6 * R, to = R, length.out = noRadii)
  } else {
    noRadii <- length(radius)
  }

  n <- dim(W)[1]
  p <- dim(W)[2]

  # Initiate the coefficient vector
  betaCorr <- matrix(nrow = p, ncol = noRadii)

  # Random starting points
  muOld <- stats::rnorm(1) # Intercept
  betaOld <- rep(0, p)

  for(r in seq_along(radius)) {
    # Iteration counter
    s <- 0; diff <- tol + 1

    while(s <= maxits & diff > tol){
      tmp1vec <- sum(y - logit(muOld + W %*% betaOld + t(betaOld) %*% sigmaUU %*% betaOld*(y - 1/2) ))
      tmp2vec <- drop(t(y - logit(muOld + W %*% betaOld + t(betaOld) %*% sigmaUU %*% betaOld * (y - 1/2) )) %*% (W + y %*% (t(betaOld) %*% sigmaUU)))
      mu <- muOld + alpha * tmp1vec
      beta <- projectOntoL1Ball(betaOld + alpha * tmp2vec, radius[r])
      diff <- sum(abs(beta - betaOld))

      muOld <- mu
      betaOld <- beta
      s <- s+1
    }
    betaCorr[, r] <- beta
  }

  value <- list(betaCorr = betaCorr, radius = radius)

  return(value)
}

