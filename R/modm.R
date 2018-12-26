#' @title Model M
#' 
#' @description  Alguma descricao a respeito do modelo M.
#' @param formula_min formula com o minimo.
#' @param formula_max formula com o maximo.
#' @export
#' @return 
#' - Ymin_est
#' - Ymax_est
#' - alpha
#' - beta
#' - Bmin 
#' - Bmax
#' - gamma
#' - delta
#' 
#' @seealso \code{\link{icm}}, \code{\link{ccrm}} and \code{\link{MinMax}}.
#' @author Pedro Rafael D. Marinho and Eufrasio de Andrade Lima Neto.
#' @references Alguma referencia do Modelo M.
#' @details Algum detalhe do modelo e/ou uso da funcao.
#' @description Descrevendo o pacote.
#' @examples 
#' data("Cardiological.MinMax", package = "iRegression") ## see Billard and Diday (2000)
#' formula_min = PulseMin ~ SystMin
#' formula_max = PulseMax ~ SystMax
#' result_modm <- modm(formula_min = formula_min, formula_max = formula_max, data = Cardiological.MinMax)
#' result_modm

modm <- function(formula_min, formula_max, data, ...) UseMethod("modm")

modm.default <- function(formula_min, formula_max, data, ...){
  
  variables1 <- all.vars(formula_min) # character vector containing all the names.
  variables2 <- all.vars(formula_max) # character vector containing all the names.  
  
  result <- modmEst_cpp(variables1, variables2, data)

  result$call <- match.call()
  class(result) <- "modm"
  return(result)
}

print.modm <- function(x, ...){
  cat("Call:\n")
  print(x$call)
  cat("\n")
  
  print(list(Bmin = x$Bmin,
             Bmax = x$Bmax, alpha = x$alpha, beta = x$beta, gamma = x$gamma))
}