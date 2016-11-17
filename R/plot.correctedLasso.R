#' @title plot.correctedLasso
#' @param x numeric number
#' @param ... other arguments
#' @examples
#' a <- 1
#' class(a) <- "lm"
#' MyHappyFunction(a)
#' @import ggplot2
#' @export
plot.correctedLasso <- function(x) {
  if(ncol(x$betaCorr) > 1) {
    df <- data.frame(radius = x$radius, nonZero = colSums(abs(x$betaCorr) > 0))
    ggplot(df, aes(radius, nonZero)) +
      geom_line() +
      labs(x = "Radius", y = "Nonzero coefficients", title = "Number of nonzero coefficients")
  } else {
    df <- data.frame(index = seq_along(x$betaCorr), beta = x$betaCorr)
    ggplot(df, aes(index, beta)) +
      geom_point() +
      labs(x = "Coefficient number", y = "Coefficient value", title = "Estimated coefficients")
  }
}
