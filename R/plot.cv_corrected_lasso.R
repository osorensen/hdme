#' @title plot.cv_corrected_lasso
#' @description Plot the output of \code{\link{cv_corrected_lasso}}.
#' @param x The object to be plotted, returned from \code{\link{cv_corrected_lasso}}.
#' @param ... Other arguments to plot (not used).
#' @export
plot.cv_corrected_lasso <- function(x, ...) {


  ggplot2::ggplot(x$cv,
                  ggplot2::aes(x = .data$radii, y = .data$mean_loss,
                                ymin = .data$lower_1se, ymax = .data$upper_1se)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(color = "gray") +
    ggplot2::labs(x = "Radius", y = "Loss", title = "Cross-validation plot for corrected lasso")
}
