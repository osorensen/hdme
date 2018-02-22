#' @title plot.cv_corrected_lasso
#' @description Plot the output of cv_corrected_lasso
#' @import ggplot2
#' @param x The object to be plotted, returned from cv_corrected_lasso
#' @param ... Other arguments to plot (not used).
#' @export
plot.cv_corrected_lasso <- function(x, ...) {
  df <- data.frame(x1 = c(0, 0),
                   x2 = c(x$radius_min, x$radius_1se),
                   y1 <- c(0, 0),
                   y2 = c(x$loss_min, x$loss_1se)
                   )

  ggplot(as.data.frame(x$cv), aes_(x =~ radii, y =~ mean_loss)) +
    geom_line() +
    geom_line(aes_(x =~ radii, y =~ lower_1se), linetype = 2) +
    geom_line(aes_(x =~ radii, y =~ upper_1se), linetype = 2) +
    labs(x = "Radius", y = "Loss", title = "Cross-validation plot") +
    geom_segment(data = df, aes_(x =~ x2, xend =~ x2, y =~ y1, yend =~ y2), color = "red") +
    geom_segment(data = df, aes_(x =~ x1, xend =~ x2, y =~ y2, yend =~ y2), color = "red") +
    geom_label(data = df, aes_(x =~ x2, y =~ y1, label =~ c("R min", "R 1se")))
}
