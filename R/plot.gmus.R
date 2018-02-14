#' @title plot.muselector
#' @import ggplot2
#' @export
plot.gmus <- function(x) {
  if(length(x$delta) > 1) {
    df <- data.frame(delta = x$delta, nonzero = x$num_non_zero)
    ggplot(df, aes(delta, nonzero)) +
      geom_line() +
      labs(x = "delta", y = "Nonzero coefficients", title = "Elbow plot")
  } else {
    df <- data.frame(index = seq_along(x$beta), beta = x$beta)
    ggplot(df, aes(index, beta)) +
      geom_point() +
      labs(x = "Coefficient number", y = "Coefficient value", title = "Estimated coefficients")
  }

}
