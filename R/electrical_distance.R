#'Electrical Distance
#'
#'Calculates the electrical distance between all nodes in the network
#'
#'  Calculates the electrical distance used in Bompard et al 2009, which is defined as \eqn{Z_{ii}+Z_{jj}-2Z_{ij}}. It takes in a matrix and outputs a matrix.
#' @param imp The impedance matrix produced by imp_ptdf
#' @export
#' @seealso \code{\link{imp_ptdf}}
electrical_distance <- function(imp){
  nums  <- ncol(imp)
  Combos <-as.matrix( expand.grid(1:nums, 1:nums))
  Z <- matrix(NA, ncol = ncol(imp), nrow = nrow(imp))
  #produces a vector with the same number of elements as the imp matrix but that measure distance
  Z[] <- imp[cbind(Combos[,1], Combos[,1])] + imp[cbind(Combos[,2], Combos[,2])] - (2*imp[Combos])
  return(Z)
}
