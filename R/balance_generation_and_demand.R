#' Balance Power network
#'
#'Ensures that the demand and generation of the power network are balanced. This is used before power
#'   flow is calculated.
#'
#' @param g An igraph object representing a power network. A character vector.
#' @param  demand The variable in g that contains the demand of each node. A character vector.
#' @param generation The variable in g that contains the generation of each node. A character vector.
#' @param output_var The desired output variable for the balanced net power demand of each node.
#'    A character vector the default is set to "net_generation".
#' @export
#' @examples
#' balance_generation_and_demand(g, demand, generation, output_var = "net_generation")
balance_generation_and_demand <- function(g, demand, generation, output_var = "net_generation"){
  #Balences the generation and demand across multiple isolated componants and removes all dead islands
  #the function takes a graph as an argument and outputs a graph
  #g: the graph of the network will have power balanced
  #demand: The Variable node_name to be used for calculating nodal demand, character vector
  #generation: The Variable node_name to be used for calculating nodal generation, character vector
  #output_var: The node_name of the variable that will be the new balanced power this is a character string
  df <- tibble(
    demand = get.vertex.attribute(g, demand) ,
    generation = get.vertex.attribute(g, generation),
    component = components(g)$membership) %>%
    group_by(component) %>%
    #Identify Deadislands and set generation and demand to 0
    mutate(DeadIsland = sum(demand)==0 | sum(generation)==0,
           demand2 = ifelse(DeadIsland, 0, demand),
           generation2 = if_else(DeadIsland, 0, generation)) %>%
    #rebalence remaining componants
    mutate(GenBal = ifelse(is.finite(sum(demand2)/sum(generation2)),sum(demand2)/sum(generation2),1),
           demand3 = ifelse(GenBal>1,
                            demand2*(sum(generation2)/sum(demand2)), demand2),
           generation3 = ifelse(GenBal<1,
                                generation2*(sum(demand2)/sum(generation2)), generation2),
           Output = generation3 - demand3
    ) %>%
    ungroup
  g_2 <- set.vertex.attribute(g, output_var, value =df$Output )
  g_2 <- delete.vertices(g_2, df$DeadIsland)
  return(g_2)
}
