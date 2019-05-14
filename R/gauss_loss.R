gauss_loss <- function(W, y, sigmaUU, beta){
  n <- nrow(W)
  loss <- numeric(ncol(beta))
  for(i in seq(ncol(beta)))
    loss[i] <- (1/n) * sum( (y - W %*% beta[, i] ) ^2 ) - t(beta[, i]) %*% sigmaUU %*% beta[, i]
  return(loss)

}
