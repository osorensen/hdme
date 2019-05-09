#' Print a Corrected Lasso object
#'
#' Default print method for a \code{corrected_lasso} object.
#'
#' @param x Fitted model object returned by \code{\link{corrected_lasso}}.
#' @param ... Other arguments (not used).
#'
#' @export
#'
#'
#'
print.corrected_lasso <- function(x, ...){
  cat("Corrected", x$family, "lasso object with", nrow(x$betaCorr), "variables fitted with",
      length(x$radii), "regularization parameters.\n")
  cat("Use functions plot() and coef() for more information about the fitted values.\n")
}

#' Print a Cross-Validated Corrected Lasso object
#'
#' Default print method for a \code{cv_corrected_lasso} object.
#'
#' @param x Fitted model object returned by \code{\link{cv_corrected_lasso}}.
#' @param ... Other arguments (not used).
#'
#' @export
#'
#'
#'
print.cv_corrected_lasso <- function(x, ...){
  cat("Cross-validation results:\n")
  print(x$cv, row.names = FALSE)
  cat("\n")
  cat("Regularization parameter at minimum loss is ", x$radius_min, " with loss ", x$loss_min, ".\n", sep = "")
  cat("Smallest regularization parameter within one standard error of minimum loss is\n",
      x$radius_1se, " with loss ", x$loss_1se, ".\n", sep = "")
}

#' Print a Generalized Dantzig Selector Object
#'
#' Default print method for a \code{gds} object.
#'
#' @param x Fitted model object returned by \code{\link{gds}}.
#' @param ... Other arguments (not used).
#'
#' @export
#'
print.gds <- function(x, ...){
  cat("Generalized Dantzig Selector with family ", x$family, ", with ", length(x$beta), " variables fitted with ",
      "regularization parameter ", x$lambda, ".\n", sep = "")
  cat("Use functions plot() and coef() for more information about the fitted values.\n")
}
