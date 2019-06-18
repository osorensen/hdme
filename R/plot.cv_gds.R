#' @title plot.cv_gds
#' @description Plot the output of \code{\link{cv_gds}}.
#' @param x The object to be plotted, returned from \code{\link{cv_gds}}.
#' @param ... Other arguments to plot (not used).
#' @export
plot.cv_gds <- function(x, ...) {

  ggplot2::ggplot(as.data.frame(x$cv),
                  ggplot2::aes_(x =~ log(lambda), y =~ mean_loss,
                                ymin =~ lower_1se,
                                ymax =~ upper_1se)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(color = "gray") +
    ggplot2::labs(x = expression(log(lambda)), y = "Deviance", title = "Cross-validation plot")
}
