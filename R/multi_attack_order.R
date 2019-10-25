#' Generate a dataframe of deletion orders
#'
#' This function takes an igraph object and returns a dataframe where each row represents a node/edge
#'    deletion order. It is used for simulating a large amount of random attacks in a reproducible way.
#' @param g an igraph object.
#' @param sims the number of attack simulations to be performed
#' @param target whether Edges or nodes will be attacked 
#' @param node_name The attribute node_name of the target
#' @export
#' @examples
#' ExampleTargets <- multi_attack_order(g, 100)
multi_attack_order <- function(g, target = "nodes", sims, node_name = "name"){
  Number_attacks <- ifelse(target == "nodes", length(V(g)), length(E(g)))
  Out <- 1:sims %>%
    map(~{
      attack_order <- random_attack(g, target, number = Number_attacks, node_name)
      df <- attack_order%>%
        as.matrix() %>%
        t %>%
        as_tibble() %>%
        setNames(., paste0("Target_", 1:length(attack_order)))
      return(df)
    }) %>%
    bind_rows() %>%
    mutate(SimulationID = paste0("Simulation_ID_", 1:nrow(.))) %>%
    select(SimulationID, everything())
  return(Out)
}
