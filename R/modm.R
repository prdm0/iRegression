modm <- function(xmin, xup, ymin, yup){
  if(any(is.na(c(xmin, xup, ymin, yup)))){
    warning("Check the arguments. NA present in at least one of the arguments.")
  }

  x_range <- (xup - xmin)/2; y_range <- (yup - ymin)/2
  s0 <- min(y_range/x_range); x_center <- (xup + xmin)/2
  y_center <- (yup + ymin)/2

  alpha <- cov(x_center, y_center)/var(x_center)
  beta <- min(s0, max(0, cov(x_range, y_range)/var(x_range)))

  Bmin <- (mean(y_center) - alpha*mean(x_center)) - (mean(y_range) - beta*mean(x_range))
  Bmax <- (mean(y_center) - alpha*mean(x_center)) + (mean(y_range) - beta*mean(x_range))

  gamma <- (Bmax + Bmin)/2; delta <- (Bmax - Bmin)/2

  Ymin_est <- alpha * x_center + beta * x_range - (gamma - delta)
  Ymax_est <- alpha * x_center + beta * x_range + (gamma - delta)

  return(list(Ymin_est = Ymin_est, Ymax_est = Ymax_est, alpha = alpha,
              beta = beta, Bmin = Bmin, Bmax = Bmax, gamma = gamma,
              delta = delta))
}
