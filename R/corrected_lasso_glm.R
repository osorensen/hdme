corrected_lasso_glm <- function(W, y, sigmaUU, family = c("binomial", "poisson"),
                                radii, no_radii, alpha, maxits, tol = 1e-10, maxIR = 50){
  family <- match.arg(family)

  if(family == "binomial") {
    mean_function <- function(eta, betaOld, sigmaUU){
      stats::plogis(eta - c(1/2 * t(betaOld) %*% sigmaUU %*% betaOld))
    }
  } else if(family == "poisson") {
    mean_function <- function(eta, betaOld, sigmaUU){
      z <- seq(from = 0, to = 20, by = 1.0)
      vapply(eta, function(etai){
        sum(z / factorial(z) * exp(z * etai - z^2 / 2 * c(t(betaOld) %*% sigmaUU %*% betaOld))) /
          sum(1 / factorial(z) * exp(z * etai - z^2 / 2 * c(t(betaOld) %*% sigmaUU %*% betaOld)))
      }, FUN.VALUE = numeric(1))
    }
  }


  if( is.null(radii) ){
    radii <- set_radius(W, y, family = family, no_radii = no_radii,
                        limit_factors = c(1e-6, 1))
  }

  no_radii <- length(radii)

  n <- dim(W)[1]
  p <- dim(W)[2]

  # Initiate the coefficient vector
  betaCorr <- matrix(nrow = p, ncol = no_radii)

  # Random starting points
  muOld <- stats::rnorm(1) # Intercept
  betaOld <- rep(0, p)

  for(r in seq_along(radii)) {
    # Iteration counter
    s <- 0
    diff <- tol + 1

    while(s <= maxIR && diff > tol){
      eta <- muOld + W %*% betaOld + y * c(t(betaOld) %*% sigmaUU %*% betaOld)
      part1 <- y - mean_function(eta, betaOld, sigmaUU)
      part2 <- W + y %*% (t(betaOld) %*% sigmaUU)
      mu <- muOld + alpha * sum(part1)
      beta <- project_onto_l1_ball(betaOld + c(alpha * t(part1) %*% (part2)), radii[r])
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

