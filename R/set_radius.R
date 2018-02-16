set_radius <- function(W, y, noRadii) {
  # First run the naive Lasso
  lassoFit <- glmnet::cv.glmnet(W, y)
  betaNaive <- glmnet::coef.cv.glmnet(lassoFit, s = "lambda.min")

  # Use the estimated vector to find the upper radius for cross-validation
  R <- 2 * sum( abs( betaNaive ) )
  # Set the cross-validation range
  radius <- seq(from = 1e-3 * R, to = R, length.out = noRadii)

}
