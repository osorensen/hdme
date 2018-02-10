#' @title cv_corrected_lasso
#' @import ggplot2
#' @export
plot.cv_corrected_lasso <- function(x) {
  df <- data.frame(x1 = c(0, 0),
                   x2 = c(x$radius.min, x$radius.1se),
                   y1 <- c(0, 0),
                   y2 = c(x$loss.min, x$loss.1se)
                   )

  ggplot(as.data.frame(x$cv), aes(radius, meanLoss)) +
    geom_line() +
    geom_line(aes(radius, lower1se), linetype = 2) +
    geom_line(aes(radius, upper1se), linetype = 2) +
    labs(x = "Radius", y = "Loss", title = "Cross-validation plot") +
    geom_segment(data = df, aes(x = x2, xend = x2, y = y1, yend = y2), color = "red") +
    geom_segment(data = df, aes(x = x1, xend = x2, y = y2, yend = y2), color = "red") +
    geom_label(data = df, aes(x = x2, y = y1, label = c("Min. CV", "1se CV")))
}
