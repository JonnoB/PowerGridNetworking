#' Generate Attack order in multiple simulations
#'
#' Takes the output from the 'multi_attack_order' function and produces an attack vector of the next simulation to be run
#'    The function is used when a large number of simulations are being run and saved to a folder for reuse.
#'    This function is usally only used as part of \code{\link{save_multi_attacks}}
#'
#' @param sim_order The dataframe produced by the multi_attack_order
#' @param folder the folder that the attack simuations are being saved to
#' @export
#' @seealso \code{\link{multi_attack_order}}
#' @examples
#' generate_attack_order(sim_order, folder)
generate_attack_order <- function(sim_order, folder){
NextSim <- next_attack_simulation(sim_order, folder)
  #The deletion order of the lowest simulation not yet completed.
  sim_order %>%
    filter(SimulationID == NextSim) %>%
    select(-SimulationID) %>%
    t %>%
  as.vector
}
