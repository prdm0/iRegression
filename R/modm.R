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
#' @seealso \code{\link{cm}}, \code{\link{ccrm}} and \code{\link{MinMax}}.
#' @author Pedro Rafael D. Marinho and Eufrasio de Andrade Lima Neto.
#' @references Alguma referencia do Modelo M.
#' @details Algum detalhe do modelo e/ou uso da funcao.
#' @description Descrevendo o pacote.

modm <- function(formula1, formula2, data, ...) UseMethod("modm")

modm.default <- function(formula_min, formula_max, data, ...){
  
  variables1 <- all.vars(formula_min) # character vector containing all the names.
  variables2 <- all.vars(formula_max) # character vector containing all the names.  
  
  result <- modmEst_cpp(variables1[2], variables2[2], variables1[1], variables2[1])
  
  return(result)
}