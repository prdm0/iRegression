cm <- function (formula1,formula2, data, ...) UseMethod("cm")

cm.default <- function (formula1, formula2, data, ...)
{
  variables1 <- all.vars(formula1) # character vector containing all the names.
  variables2 <- all.vars(formula2) # character vector containing all the names.
  
  # cmEst_cpp is a function written in C++ using RcppArmadillo.
  est_cpp <- cmEst_cpp(variables1, variables2, data)
  
  est_cpp$call <- match.call()
  class(est_cpp) <- "cm"
  return(est_cpp)
}

print.cm <- function(x, ...)
{
  cat("Call:\n")
  print(x$call)
  cat("\n")
  names(x$coefficients) <- c("(Intercept)", paste0("beta_", 1:(length(ex.cm$coefficients) - 1)))
  print(list(coefficients = x$coefficients,
             sigma = x$sigma, df = x$df))
}

summary.cm <- function(object, ...)
{
  rmse.l <- sqrt(mean(object$residuals.l^2))
  rmse.u <- sqrt(mean(object$residuals.u^2))
  se <- sqrt(diag(object$vcov))
  tval <- coef(object) / se
  TAB <- cbind(Estimate = coef(object),
               StdErr = se)
  res <- list(call=object$call,
              coefficients=TAB,
              RMSE.l = rmse.l,
              RMSE.u = rmse.u)
  class(res) <- "summary.cm"
  res
}

print.summary.cm <- function(x, ...)
{
  cat("Call:\n")
  print(x$call)
  cat("\n")
  rownames(x$coefficients)[-1] <- seq(1,length(rownames(x$coefficients)[-1]))
  print(x$coefficients)
  cat("\n")
  cat("RMSE.L:\n")
  print(x$RMSE.l)
  cat("RMSE.U:\n")
  print(x$RMSE.u)
}

fitted.cm <- function(object, ...)
{
  fit.Min <- object$fitted.values.l
  fit.Max <- object$fitted.values.u
  ftd <- cbind(fit.Min,
               fit.Max)
  fitted <- round(ftd, digits=3)
  class(fitted) <- "fitted.cm"
  fitted
}

residuals.cm <- function (object, ...)
{
  resid.Min <- object$residuals.l	
  resid.Max <- object$residuals.u
  resi <- cbind(resid.Min,resid.Max)
  resi <- round(resi,digits=3)
  class(resi) <- "residuals.cm"
  resi
}

cm.formula <- function(formula1,formula2,data=list(),...)
{
  mf1 <- model.frame(formula=formula1,data=data)
  x1 <- model.matrix(attr(mf1, "terms"), data=mf1)
  y1 <- model.response(mf1)
  mf2 <- model.frame(formula=formula2,data=data)
  x2 <- model.matrix(attr(mf2, "terms"), data=mf2)
  y2 <- model.response(mf2)
  est <- cm.default(formula1, formula2, data,...)
  est$call <- match.call()
  est$formula1 <- formula1
  est$formula2 <- formula2
  est
}
