#' Algorithm for mus
#'
#' @param W Matrix of measurements.
#' @param y Response vector.
#' @param lambda Regularization parameter due to residual.
#' @param delta Regularization parameter due to measurement error.
#'
musalgorithm <- function(W, y, lambda, delta){
  # We assume the first column of W is constants, i.e., intercept
  n <- dim(W)[1]
  p <- dim(W)[2]
  obj <- c(rep(1,p),rep(0,p))
  mat <- matrix(0,nrow=4*p, ncol=2*p)

  # Inequality constraint, -u_j - beta_j <= 0
  mat[1:p,1:p] <- -diag(p)
  mat[1:p,(p+1):(2*p)] <- -diag(p)

  # Inequality constraint, -u_j + beta_j <= 0
  mat[(p+1):(2*p),1:p] <- -diag(p)
  mat[(p+1):(2*p),(p+1):(2*p)] <- diag(p)

  # First "score function" constraint
  mat[(2*p+1),1:p] <- matrix(0, nrow=1, ncol=p)
  mat[(2*p+2):(3*p),1:p] <- matrix(-delta, nrow=(p-1), ncol=p)
  mat[(2*p+1):(3*p),(p+1):(2*p)] <- 1/n*(t(W)%*%W)

  # Second "score function" constraint
  mat[(3*p+1),1:p] <- matrix(0, nrow=1, ncol=p)
  mat[(3*p+2):(4*p),1:p] <- matrix(-delta, nrow=(p-1), ncol=p)
  mat[(3*p+1):(4*p),(p+1):(2*p)] <- -1/n*(t(W)%*%W)

  rhs <- rep(0,(4*p))
  rhs[(2*p+1)] <- 1/n*(t(W[,1])%*%y)
  rhs[(2*p+2):(3*p)] <- lambda + 1/n*(t(W[,-1])%*%y)
  rhs[(3*p+1)] <- -1/n*(t(W[,1])%*%y)
  rhs[(3*p+2):(4*p)] <- lambda - 1/n*(t(W[,-1])%*%y)
  dir <- rep("<=",4*p)
  bounds <- list(lower=list(ind=1:(2*p), val=rep(-Inf,2*p)),
                 upper=list(ind=1:(2*p), val=rep(Inf,2*p)))


  if (requireNamespace("Rglpk", quietly = TRUE)) {
    bhat <- Rglpk::Rglpk_solve_LP(obj = obj, mat = mat, dir = dir,
                                  rhs = rhs, bounds = bounds)$solution
  } else if (requireNamespace("lpSolveAPI", quietly = TRUE)) {
    #message("Package Rglpk not found. Using package lpSolveAPI instead.")
    # If Rglpk is not installed, but lpSolveAPI is, then use that instead
    lpmodel <- lpSolveAPI::make.lp(nrow = nrow(mat), ncol = ncol(mat))

    for(col in seq(1, ncol(mat), by = 1)) {
      lpSolveAPI::set.column(lpmodel, col, mat[, col])
    }

    lpSolveAPI::set.rhs(lpmodel, rhs)
    lpSolveAPI::set.bounds(lpmodel, lower = bounds$lower$val, upper = bounds$upper$val)
    lpSolveAPI::set.objfn(lpmodel, obj)
    lpSolveAPI::set.constr.type(lpmodel, dir)

    status <- lpSolveAPI::solve.lpExtPtr(lpmodel)

    bhat <- lpSolveAPI::get.variables(lpmodel)
    #lpSolveAPI::delete.lp(lpmodel)
  } else {
    stop("Package Rglpk or lpSolveAPI needed for this function to work. Please install one of them.",
         call. = FALSE)
  }

  return(bhat[(p+1):(2*p)])


}
