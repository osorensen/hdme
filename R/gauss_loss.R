gauss_loss <- function(W, y, sigmaUU, beta){
  n <- nrow(W)

  if(is.null(ncol(beta))){
    return((1/n) * sum( (y - W %*% beta )^2 ) - t(beta) %*% sigmaUU %*% beta)
  } else {
    loss <- numeric(ncol(beta))
    for(i in seq(ncol(beta)))
      loss[i] <- (1/n) * sum( (y - W %*% beta[, i] ) ^2 ) - t(beta[, i]) %*% sigmaUU %*% beta[, i]
    return(loss)
  }
}
