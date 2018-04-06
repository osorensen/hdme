#' @title plot.cv_corrected_lasso
#' @description Plot the output of cv_corrected_lasso
#' @param x The object to be plotted, returned from cv_corrected_lasso
#' @param ... Other arguments to plot (not used).
#' @export
plot.cv_corrected_lasso <- function(x, ...) {
  df <- data.frame(x1 = c(0, 0),
                   x2 = c(x$radius_min, x$radius_1se),
                   y1 <- c(0, 0),
                   y2 = c(x$loss_min, x$loss_1se)
                   )

  ggplot2::ggplot(as.data.frame(x$cv), ggplot2::aes_(x =~ radii, y =~ mean_loss)) +
    ggplot2::geom_line() +
    ggplot2::geom_line(ggplot2::aes_(x =~ radii, y =~ lower_1se), linetype = 2) +
    ggplot2::geom_line(ggplot2::aes_(x =~ radii, y =~ upper_1se), linetype = 2) +
    ggplot2::labs(x = "Radius", y = "Loss", title = "Cross-validation plot") +
    ggplot2::geom_segment(data = df, ggplot2::aes_(x =~ x2, xend =~ x2, y =~ y1, yend =~ y2), color = "red") +
    ggplot2::geom_segment(data = df, ggplot2::aes_(x =~ x1, xend =~ x2, y =~ y2, yend =~ y2), color = "red") +
    ggplot2::geom_label(data = df, ggplot2::aes_(x =~ x2, y =~ y1, label =~ c("R min", "R 1se")))
}
