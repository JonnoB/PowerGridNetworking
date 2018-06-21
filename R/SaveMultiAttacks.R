#' Save attack simulations to folder
#'
#'When creating many attack simulations it can be conveniant to save the resulting graphs to a folder
#'    so that re-runs are not required if further analysis is needed. This function saves the
#'    results of AttackTheGrid, and also allows the process to be interrupted.
#'    When the process is started again after and interuption the function finds the last
#'    save attack and starts the simulation process from there.
#' @param g An igraph object representing a power grid.
#' @param AttackVectors A dataframe. This is the output of the MultiAttackOrder function. If you expect.
#'    to interrupt the function this dataframe should be saved or the random seed used to generate the dataset
#'    recorded.
#' @param folder the folder data will be saved to.
#' @param MinMaxComp See AttackTheGrid. Default is 0
#' @param TotalAttackRounds See AttackTheGrid. Default is 10
#' @param CascadeMode See AttackTheGrid. Default is set to FALSE
#' @keywords multi-attack,
#' @export
#' @seealso \code{\link{AttackTheGrid}}, \code{\link{MultiAttackOrder}}
#' @example
#' SaveMultiAttacks(g, AttackVectors, folder, CascadeMode = F)

SaveMultiAttacks <- function(g, AttackVectors, folder, MinMaxComp = 0, TotalAttackRounds = 10, CascadeMode = FALSE){

  #set working directory
  gc()
  print("Calculating first simulation")
  TimeAtFirstSimulation <- Sys.time()
  for (i in 1:nrow(AttackVectors)) {
    #walk was used previously but it created a crash.
    #The crash would happen approx every 30 simulations
    #I can't replicate the crash using dummy data so am leaving it.

    NextSim <- NextAttackSimulation(AttackVectors, folder)
    DeletionOrder <- GenerateAttackOrder(AttackVectors, folder)

    FixedNodes <- quo(FixedStrategyAttack(g, DeletionOrder))
    T1 <- Sys.time()
    #suppres attack the grid messages
    AttackSeries <- suppressMessages(AttackTheGrid(list(list(g)),
                                                   FixedNodes,
                                                   referenceGrid = NULL,
                                                   MinMaxComp,
                                                   TotalAttackRounds,
                                                   CascadeMode))

    saveRDS(AttackSeries, file = file.path(folder, paste0(NextSim, ".rds")))
    rm(AttackSeries)
    gc()
    T2<- Sys.time()
    SimulationRoundTime <- round(difftime(T2, T1, units = "mins" ))
    TimeToCompletion <- (difftime(T2, TimeAtFirstSimulation)/i)*(nrow(AttackVectors)-i)
    ExpectedCompletionTime<- T2 + TimeToCompletion
    TimeUnit<- ifelse(as.numeric(TimeToCompletion, units= "hours")<1, "mintues", "hours")

    print(paste("Time taken for simulation", i, "is",
                SimulationRoundTime ,
                "minutes. Est time to completion",
                round(as.numeric(TimeToCompletion, units = TimeUnit)),
                TimeUnit,
                "Est completion time is",
                ExpectedCompletionTime))
  }
  TimeAtLastSimulation <- Sys.time()

  print(paste("Time taken for all simulations is", round(difftime(TimeAtLastSimulation, TimeAtFirstSimulation , units = "hours" ),2), TimeUnit))

}
