#' Generalized Matrix Uncertainty Lasso
#' @description Generalized Matrix Uncertainty Lasso. This is a worker function
#'   for which an interface will be added later.
#' @param W Design matrix, measured with error. Must be a numeric matrix.
#' @param y Vector of responses.
#' @param lambda Regularization parameter.
#' @param delta Additional regularization parameter, bounding the measurement
#'   error.
#' @param activeSet Logical. Whether to use an active set strategy.
#' @return Intercept and coefficients at the values of lambda and delta
#'   specified.
#' @references \insertRef{rosenbaum2010}{hdme} \insertRef{sorensen2018}{hdme}
#'
gmu_lasso_binomial <- function(W, y, lambda = NULL, delta = NULL, activeSet=FALSE){
    # We assume the first column in W takes the intercept
    n <- dim(W)[1]
    p <- dim(W)[2]
    bOld <- stats::rnorm(p)/p
    bNew <- stats::rnorm(p)/p
    IRLSeps <- 1e-7
    maxit <- 100
    count <- 1
    Diff1 <- 1
    Diff2 <- 1
    mu <- logit
    dmu <- dlogit

    while(Diff1 > IRLSeps & Diff2 > IRLSeps & count < maxit){
        bOlder <- bOld
        bOld <- bNew
        V <- dmu(W%*%bOld)
        z <- W%*%bOld + (y - mu(W%*%bOld))/dmu(W%*%bOld)
        Wtilde <- c(sqrt(V))*W
        ztilde <- c(sqrt(V))*c(z)
        gamma <- delta*sqrt(sum(V^2))/2/sqrt(n)
        omega <- rep(0,p)
        omega[-1] <- sapply(2:p, function(x) { lambda + gamma * sum(abs(bOld[-x])) })

        bNew <- fit_mu_lasso(omega, gamma, Wtilde, ztilde, bOld, activeSet)
        count <- count+1
        Diff1 <- sum(abs(bNew - bOld))
        Diff2 <- sum(abs(bNew - bOlder))
        print(paste("Diff1 = ", Diff1, ", Diff2 = ", Diff2, sep=""))
    }
    if(count >= maxit) print(paste("Did not converge"))
    return(bNew)
}
