#' Cross-Validated Generalized Dantzig Selector
#' @description Generalized Dantzig Selector with cross-validation.
#'
#' @param X Design matrix.
#' @param y Vector of the continuous response value.
#' @param family Use "gaussian" for linear regression, "binomial" for logistic
#'   regression and "poisson" for Poisson regression.
#' @param no_lambda Length of the vector \code{lambda} of regularization
#'   parameters. Note that if \code{lambda} is not provided, the actual number
#'   of values might differ slightly, due to the algorithm used by
#'   \code{glmnet::glmnet} in finding a grid of \code{lambda} values.
#' @param lambda Regularization parameter. If not supplied and if
#'   \code{no_lambda > 1}, a sequence of \code{no_lambda} regularization
#'   parameters is computed with \code{glmnet::glmnet}. If \code{no_lambda = 1}
#'   then the cross-validated optimum for the lasso is computed using
#'   \code{glmnet::cv.glmnet}.
#' @param n_folds Number of cross-validation folds to use.
#' @param weights A vector of weights for each row of \code{X}. Defaults to 1
#'   per observation.
#' @return An object of class \code{cv_gds}.
#'
#' @details Cross-validation loss is calculated as the deviance of the model divided
#' by the number of observations.
#' For the Gaussian case, this is the mean squared error. Weights supplied
#' through the \code{weights} argument are used both in fitting the models
#' and when evaluating the test set deviance.
#'
#' @references \insertRef{candes2007}{hdme}
#'
#'   \insertRef{james2009}{hdme}
#'
#' @examples
#' \dontrun{
#' # Example with logistic regression
#' n <- 1000  # Number of samples
#' p <- 10 # Number of covariates
#' X <- matrix(rnorm(n * p), nrow = n) # True (latent) variables # Design matrix
#' beta <- c(seq(from = 0.1, to = 1, length.out = 5), rep(0, p-5)) # True regression coefficients
#' y <- rbinom(n, 1, (1 + exp(-X %*% beta))^(-1)) # Binomially distributed response
#' cv_fit <- cv_gds(X, y, family = "binomial", no_lambda = 50, n_folds = 10)
#' print(cv_fit)
#' plot(cv_fit)
#'
#' # Now fit a single GDS at the optimum lambda value determined by cross-validation
#' fit <- gds(X, y, lambda = cv_fit$lambda_min, family = "binomial")
#' plot(fit)
#'
#' # Compare this to the fit for which lambda is selected by GDS
#' # This automatic selection is performed by glmnet::cv.glmnet, for
#' # the sake of speed
#' fit2 <- gds(X, y, family = "binomial")
#'
#' The following plot compares the two fits.
#' library(ggplot2)
#' library(tidyr)
#' df <- data.frame(fit = fit$beta, fit2 = fit2$beta, index = seq(1, p, by = 1))
#' ggplot(gather(df, key = "Model", value = "Coefficient", -index),
#'        aes(x = index, y = Coefficient, color = Model)) +
#'        geom_point() +
#'        theme(legend.title = element_blank())
#'
#' }
#'
#' @export
cv_gds <- function(X, y, family = "gaussian", no_lambda = 10, lambda = NULL,
                n_folds = 5, weights = rep(1, length(y))) {

  stopifnot(family %in% c("gaussian", "poisson", "binomial"))

  # Define lambda if user has not specified it
  if(is.null(lambda)){
    if(no_lambda == 1){
      lambda <- glmnet::cv.glmnet(X, y, family = family, weights = weights)$lambda.min
    } else if(no_lambda > 1){
      lambda <- glmnet::glmnet(X, y, family = family, nlambda = no_lambda, weights = weights)$lambda
      no_lambda <- length(lambda)
    }
  }

  stopifnot(all(lambda >= 0))

  # Set up cross-validation if necessary
  n <- nrow(X)
  cv_list <- set_up_cv(n, n_folds)

  loss <- matrix(nrow = no_lambda, ncol = n_folds)

  for(i in seq(n_folds)) {
    test <- (cv_list$fold_id == i)

    # Deviance along lambda
    loss[, i] <- as.numeric(lapply(lambda, function(l){
      fit <- gmus(W = X[!test, , drop = FALSE], y = y[!test], lambda = l, delta = 0,
                  family = family, weights = weights[!test])

      linpred <- X[test, , drop = FALSE] %*% fit$beta + fit$intercept
      if(family == "binomial"){
        test_pred <- stats::plogis(linpred)
      } else if(family == "gaussian"){
        test_pred <- linpred
      } else if(family == "poisson"){
        test_pred <- exp(linpred)
      }

      deviance(y[test], test_pred, family, weights[test])
      })) / sum(test) # Divide by size of test set to get mean deviance
    }

    cv <- data.frame(
      lambda = lambda,
      mean_loss = rowMeans(loss),
      sd_loss = apply(loss, 1, stats::sd),
      upper_1se = rowMeans(loss) + apply(loss, 1, stats::sd) / sqrt(n_folds),
      lower_1se = rowMeans(loss) - apply(loss, 1, stats::sd) / sqrt(n_folds)
    )

    ind1 <- min(which(cv$mean_loss == min(cv$mean_loss)))
    lambda_min <- cv$lambda[ind1]
    loss_min <- cv$mean_loss[ind1]

    ind2 <- min(which(min(cv$mean_loss) > cv$lower_1se))
    lambda_1se <- cv$lambda[ind2]
    loss_1se <- cv$mean_loss[ind2]

    fit <- list(cv = cv,
                lambda_min = lambda_min,
                loss_min = loss_min,
                lambda_1se = lambda_1se,
                loss_1se = loss_1se,
                family = family
    )

  class(fit) <- "cv_gds"
  return(fit)
}



