project_onto_l1_ball <- function(gradient, radius) {
  if(radius < 0) stop("Radius should be non-negative")
  u <- sort(abs(gradient), decreasing = TRUE)
  sv <- cumsum(u)
  rho <- max(which(u > (sv-radius)/(1:length(u))))
  beta <- max(0, (sv[rho] - radius)/rho)
  w <- as.numeric(sign(gradient) * as.numeric(lapply(abs(gradient) - beta, max,0)))
  return(w)
}
