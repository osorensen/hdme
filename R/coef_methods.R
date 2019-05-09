#' Extract Coefficients of a Corrected Lasso object
#'
#' Default coef method for a \code{corrected_lasso} object.
#'
#' @param object Fitted model object returned by \code{\link{corrected_lasso}}.
#' @param ... Other arguments (not used).
#'
#' @export
coef.corrected_lasso <- function(object, ...){
  if(length(object$radii) > 1){
    cat("Number of nonzero coefficient estimates as a function of regularization parameter (radius):\n")
    print(data.frame(
      radius = fit$radii,
      nonzeros = apply(object$betaCorr, 2, function(x) sum(x != 0))
    ), row.names = FALSE)
  } else if(length(object$radii) == 1 & !all(object$betaCorr == 0)){
    cat("Non-zero coefficients:\n")
    print(data.frame(
      coefficient = which(object$betaCorr != 0),
      estimate = object$betaCorr[object$betaCorr != 0]
    ), row.names = FALSE)
  }

}


#' Extract Coefficients of a Generalized Dantzig Selector Object
#'
#' Default coef method for a \code{gds} object.
#'
#' @param object Fitted model object returned by \code{\link{gds}}.
#' @param all Logical indicating whether to show all coefficient estimates, or only non-zeros.
#' @param ... Other arguments (not used).
#'
#' @export
coef.gds <- function(object, all = FALSE, ...){

  if(!all){
    cat("Non-zero coefficients:\n")
    print(data.frame(
      coefficient = which(object$beta != 0),
      estimate = object$beta[object$beta != 0]
    ), row.names = FALSE)
  } else{
    cat("Coefficient estimates:\n")
    print(data.frame(
      coefficient = 1:length(object$beta),
      estimate = object$beta
    ), row.names = FALSE)
  }


}
