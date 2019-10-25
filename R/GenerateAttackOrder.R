#' Generate Attack order in multiple simulations
#'
#' Takes the output from the 'MultiAttackOrder' function and produces an attack vector of the next simulation to be run
#'    The function is used when a large number of simulations are being run and saved to a folder for reuse.
#'    This function is usally only used as part of \code{\link{SaveMultiAttacks}}
#'
#' @param SimOrder The dataframe produced by the MultiAttackOrder
#' @param folder the folder that the attack simuations are being saved to
#' @export
#' @seealso \code{\link{MultiAttackOrder}}, \code{\link{SaveMultiAttacks}}
#' @examples
#' GenerateAttackOrder(SimOrder, folder)



GenerateAttackOrder <- function(SimOrder, folder){

NextSim <- NextAttackSimulation(SimOrder, folder)

  #The deletion order of the lowest simulation not yet completed.
  SimOrder %>%
    filter(SimulationID == NextSim) %>%
    select(-SimulationID) %>%
    t %>%
  as.vector
}
