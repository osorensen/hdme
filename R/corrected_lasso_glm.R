#' @import stats
#' @import glmnet
corrected_lasso_glm <- function(W, y, sigmaUU, family = c("binomial", "poisson"),
                                radii, no_radii, alpha, maxits, standardize, tol = 1e-10, maxIR = 50){
  family <- match.arg(family)

  if(family == "binomial") {
    mean_function <- logit
  } else if(family == "poisson") {
    mean_function <- pois
  }


  if( is.null(radii) ){
    # First run the naive Lasso
    lassoFit <- cv.glmnet(W, y, family = family)
    betaNaive <- coef.cv.glmnet(lassoFit, s = "lambda.min")

    no_radii <- 20
    # Use the estimated vector to find the upper radii for cross-validation
    R <- sum( abs( betaNaive ) )
    # Set the cross-validation range
    radii <- seq(from = 1e-6 * R, to = R, length.out = no_radii)
  } else {
    no_radii <- length(radii)
  }

  n <- dim(W)[1]
  p <- dim(W)[2]

  # Initiate the coefficient vector
  betaCorr <- matrix(nrow = p, ncol = no_radii)

  # Random starting points
  muOld <- rnorm(1) # Intercept
  betaOld <- rep(0, p)

  for(r in seq_along(radii)) {
    # Iteration counter
    s <- 0
    diff <- tol + 1
    #cat("Step", r, "of outer iteration loop. Radius =", radii[r], "\n")

    while(s <= maxIR & diff > tol){
      tmp1vec <- sum(y - mean_function(muOld + W %*% betaOld + (y - 1/2) * as.vector(t(betaOld) %*% sigmaUU %*% betaOld )))

      part1 <- y - mean_function(muOld + W %*% betaOld + (y - 1/2) * as.vector(t(betaOld) %*% sigmaUU %*% betaOld) )
      part2 <- W + y %*% (t(betaOld) %*% sigmaUU)
      tmp2vec <- as.vector(t(part1) %*% (part2))
      mu <- muOld + alpha * tmp1vec
      beta <- project_onto_l1_ball(betaOld + alpha * tmp2vec, radii[r])
      diff <- sum(abs(beta - betaOld))

      muOld <- mu
      betaOld <- beta
      s <- s+1
    }
    betaCorr[, r] <- beta
  }

  value <- list(betaCorr = betaCorr, radii = radii)

  return(value)
}

