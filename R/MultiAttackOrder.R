#' Generate a dataframe of deletion orders
#' 
#'    This function takes an igrap object and returns a dataframe where each row represents a node/edge 
#'    deletion order. It is used for simulating a large amount of attacks in a reproducible way.
#'    Currently the function only generates random attacks.
#'    
#'    @param g an igraph object.
#'    @param Sims the number of attack simulations to be performed
#'    @keywords 
#'    @export
#'    @examples 
#'    ExampleTargets <- MultiAttackOrder(g, 100)


MultiAttackOrder <- function(g, Sims){
  Out <- 1:Sims %>%
    map(~{
      AttackOrder <- RandomAttack(g, Number = length(V(g))) 
      df <- AttackOrder%>%
        as.matrix() %>% 
        t %>%
        as_tibble() %>%
        setNames(., paste0("Target_", 1:length(AttackOrder)))
      return(df)
    }) %>%
    bind_rows() %>%
    mutate(SimulationID = paste0("Simulation_ID_", 1:nrow(.))) %>%
    select(SimulationID, everything())
  return(Out)
}