#' Generate Attack order in multiple simulations
#' 
#' Takes the output from the 'MultiAttackOrder' function and produces an attack vector of the next simulation to be run
#' The function is used when a large number of simulations are being run and saved to a folder for reuse.
#' @param SimOrder The dataframe produced by the MultiAttackOrder
#' @param foler the folder that the attack simuations are being saved to
#' @keywords
#' @export
#' @seealso MultiAttackOrder
#' @examples
#' GenerateAttackOrder(SimOrder, folder)



GenerateAttackOrder <- function(SimOrder, folder){
  
  CurrentSims <- list.files(folder) %>%
    str_replace_all(., "\\D", "" )
  
  NeededSims <- SimOrder$SimulationID %>%
    str_replace_all(., pattern ="\\D", "" ) %>%
    as.numeric()
  
  #The simulations still required in current run.
  NeededSims2 <- NeededSims[!(NeededSims %in% CurrentSims)]
  
  #The deletion order of the lowest simulation not yet completed.
  SimOrder %>%
    filter(SimulationID == paste0("Simulation_ID_", min(NeededSims2))) %>%
    select(-SimulationID) %>%
    t
}