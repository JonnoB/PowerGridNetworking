#'Electrical Distance
#'
#'Calculates the electrical distance between all nodes in the network
#'
#'  Calculates the electrical distance used in Bompard et al 2009, which is defined as \eqn{Z_{ii}+Z_{jj}-2Z_{ij}}. It takes in a matrix and outputs a matrix.
#' @param Imp The impedance matrix produced by ImpPTDF
#' @export
#' @seealso \code{\link{ImpPTDF}}
ElectricalDistance <- function(Imp){

  nums  <- ncol(Imp)
  Combos <-as.matrix( expand.grid(1:nums, 1:nums))

  Z <- matrix(NA, ncol = ncol(Imp), nrow = nrow(Imp))

  #produces a vector with the same number of elements as the Imp matrix but that measure distance
  Z[] <- Imp[cbind(Combos[,1], Combos[,1])] + Imp[cbind(Combos[,2], Combos[,2])] - (2*Imp[Combos])




  return(Z)

}

