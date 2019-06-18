gauss_loss <- function(W, y, sigmaUU, beta){
  n <- nrow(W)
  loss <- numeric(ncol(beta))
  for(i in seq(ncol(beta)))
    loss[i] <- (1/n) * sum( (y - W %*% beta[, i] ) ^2 ) - t(beta[, i]) %*% sigmaUU %*% beta[, i]
  return(loss)

}

deviance <- function(y, yhat, family, weights){
  if(family == "binomial"){
    # https://stats.stackexchange.com/questions/161113/how-to-compute-deviance-statistic-for-a-simple-logistic-regression-model-in-the
    x1 <- y * log(y / yhat)
    x1[is.na(x1) | x1 == -Inf] <- 0

    x2 <- (1 - y) * log((1 - y)/(1 - yhat))
    x2[is.na(x2) | x2 == -Inf] <- 0

    return(sum((x1 + x2) * weights) * 2)
  } else if(family == "gaussian"){
    return(sum((y - yhat)^2 * weights))
  } else if(family == "poisson"){
    x1 <- y * log(y / yhat)
    x1[is.na(x1) | x1 == -Inf] <- 0

    x2 <- (y - yhat)
    x2[is.na(x2) | x2 == -Inf] <- 0

    return(2 * sum(weights * (x1 - x2)))
  } else{
    cat("Unknown family", family, ".\n")
    stop()
  }
}


