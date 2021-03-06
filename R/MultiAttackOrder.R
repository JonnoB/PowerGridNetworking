#' Generate a dataframe of deletion orders
#'
#' This function takes an igraph object and returns a dataframe where each row represents a node/edge
#'    deletion order. It is used for simulating a large amount of random attacks in a reproducible way.
#' @param g an igraph object.
#' @param Sims the number of attack simulations to be performed
#' @param Target whether Edges or Nodes will be attacked 
#' @param Name The attribute name of the target
#' @export
#' @examples
#' ExampleTargets <- MultiAttackOrder(g, 100)


MultiAttackOrder <- function(g, Target = "Nodes", Sims, Name = "name"){
  
  Number_attacks <- ifelse(Target == "Nodes", length(igraph::V(g)), length(igraph::E(g)))
  
  Out <- 1:Sims %>%
    purrr::map(~{
      AttackOrder <- RandomAttack(g, Target, Number = Number_attacks, Name)
      df <- AttackOrder%>%
        as.matrix() %>%
        t %>%
        dplyr::as_tibble() %>%
        stats::setNames(., paste0("Target_", 1:length(AttackOrder)))
      return(df)
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(SimulationID = paste0("Simulation_ID_", 1:nrow(.))) %>%
    dplyr::select(SimulationID, dplyr::everything())
  return(Out)
}
