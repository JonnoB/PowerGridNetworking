#' Save attack simulations to folder
#'
#'When creating many attack simulations it can be conveniant to save the resulting graphs to a folder
#'    so that re-runs are not required if further analysis is needed. This function saves the
#'    results of AttackTheGrid, and also allows the process to be interrupted.
#'    When the process is started again after and interuption the function finds the last
#'    save attack and starts the simulation process from there.
#' @param g An igraph object representing a powet grid.
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

  1:nrow(AttackVectors) %>% walk(~{

    NextSim <- NextAttackSimulation(AttackVectors, folder)
    DeletionOrder <- GenerateAttackOrder(AttackVectors, folder)

    FixedNodes <- quo(FixedStrategyAttack(g, DeletionOrder))

    AttackSeries <- AttackTheGrid(list(list(g)), FixedNodes, referenceGrid = NULL, MinMaxComp, TotalAttackRounds, CascadeMode)

    saveRDS(AttackSeries, file = file.path(folder, paste0(NextSim, ".rds")))
    rm(AttackSeries)
    gc()
  }
  )
}
