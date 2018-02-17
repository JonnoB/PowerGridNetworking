#' Find the next attack simulation to be run
#' 
#' When multiple attack simulations are being performed the operation can be interrupted.
#'   This function searches the folder where the simulations are being stored and returns
#'   the next attack simulation. 
#'   @param SimOrder The output dataframe of MultiAttackOrder
#'   @param folder The folder where the attack simulations are saved
#'   @export
#'   @seealso MultiAttackOrder
#'   @example 
#'   NextAttackSimulation(SimOrder, folder)
#'   

NextAttackSimulation <- function(SimOrder, folder){

CurrentSims <- list.files(folder) %>%
  str_replace_all(., "\\D", "" )

NeededSims <- SimOrder$SimulationID %>%
  str_replace_all(., pattern ="\\D", "" ) %>%
  as.numeric()

#The simulations still required in current run.
NeededSims2 <- NeededSims[!(NeededSims %in% CurrentSims)]

#The deletion order of the lowest simulation not yet completed.
paste0("Simulation_ID_", min(NeededSims2))
       
       }