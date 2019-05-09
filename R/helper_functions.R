.onUnload <- function (libpath) {
  library.dynam.unload("hdme", libpath)
}

# Logistic functions
logit <- function(x) (1+exp(-x))^(-1)
dlogit <- function(x) exp(-x)*(1+exp(-x))^(-2)

# Poisson functions
pois <- function(x) exp(x)
dpois <- function(x) exp(x)



set_radius <- function(W, y, family = "gaussian", no_radii,
                       limit_factors = c(2e-3, 2)) {

  no_radii <- ifelse(is.null(no_radii), 20, no_radii)

  # First run the naive Lasso
  lassoFit <- glmnet::cv.glmnet(W, y, family = family)
  betaNaive <- glmnet::coef.cv.glmnet(lassoFit, s = "lambda.min")

  # Use the estimated vector to find the upper radius for cross-validation
  a <- sum( abs( betaNaive ) )
  # Set the cross-validation range
  radius <- seq(from = limit_factors[1] * a, to = limit_factors[2] * a,
                length.out = no_radii)

}

#
# get_radii <- function(W, y, family, no_radii,
#                       lower_limit_factor = 1e-6, upper_limit_factor = 2) {
#   no_radii <- ifelse(is.null(no_radii), 20, no_radii)
#   # First run the naive Lasso
#   lassoFit <- glmnet::cv.glmnet(W, y, family = family)
#   betaNaive <- glmnet::coef.cv.glmnet(lassoFit, s = "lambda.min")
#
#   # Use the estimated vector to find the upper radii for cross-validation
#   ul <- upper_limit_factor * sum( abs( betaNaive ) )
#   ll <- lower_limit_factor * sum( abs( betaNaive ) )
#
#   # Set the cross-validation range
#   seq(from = ll, to = ul, length.out = no_radii)
# }
