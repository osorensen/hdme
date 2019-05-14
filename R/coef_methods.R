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
    cat("Number of nonzero coefficient estimates\nas a function of regularization parameter (radius):\n")
    print(data.frame(
      radius = object$radii,
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


gmu_coefs <- function(object, all){
  if(length(object$delta) > 1){
    cat("Number of nonzero coefficient estimates\nas a function of regularization parameters\n(lambda, delta):\n")
    print(data.frame(
      lambda = round(object$lambda, 3),
      delta = object$delta,
      nonzeros = apply(object$beta, 2, function(x) sum(x != 0))
    ), row.names = FALSE)
  } else if(length(object$delta) == 1 & !all(object$beta == 0)){
    if(!all){
      cat("Non-zero coefficient estimates at\nregularization parameters (lambda, delta) = (",
          round(object$lambda, 3), ", ", round(object$delta, 3), "):\n", sep = "")
      print(data.frame(
        coefficient = which(object$beta != 0),
        estimate = object$beta[object$beta != 0]
      ), row.names = FALSE)
    } else {
      cat("Coefficient estimates at regularization\nparameters (lambda, delta) = (",
          round(object$lambda, 3), ", ", round(object$delta, 3), "):\n", sep = "")
      print(data.frame(
        coefficient = 1:length(object$beta),
        estimate = object$beta
      ), row.names = FALSE)
    }

  }
}

#' Extract Coefficients of a GMU Lasso object
#'
#' Default coef method for a \code{gmu_lasso} object.
#'
#' @param object Fitted model object returned by \code{\link{gmu_lasso}}.
#' @param all Logical indicating whether to show all coefficient estimates, or
#'   only non-zeros. Only used when delta is a single value.
#' @param ... Other arguments (not used).
#'
#' @export
coef.gmu_lasso <- function(object, all = FALSE, ...){
  gmu_coefs(object, all)
}


#' Extract Coefficients of a GMUS object
#'
#' Default coef method for a \code{gmus} object.
#'
#' @param object Fitted model object returned by \code{\link{gmus}}.
#' @param all Logical indicating whether to show all coefficient estimates, or
#'   only non-zeros. Only used when delta is a single value.
#' @param ... Other arguments (not used).
#'
#' @export
coef.gmus <- function(object, all = FALSE, ...){
  gmu_coefs(object, all)

}
