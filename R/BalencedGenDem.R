#' Balance Power network
#'
#'Ensures that the demand and generation of the power network are balanced. This is used before power
#'   flow is calculated.
#'
#' @param g An igraph object representing a power network. A character vector.
#' @param  DemandVar The variable in g that contains the demand of each node. A character vector.
#' @param GenerationVar The variable in g that contains the generation of each node. A character vector.
#' @param OutputVar The desired output variable for the balanced net power demand of each node.
#'    A character vector the default is set to "BalencedPower".
#' @export
#' @examples
#' BalencedGenDem(g, DemandVar, GenerationVar, OutputVar = "BalencedPower")
BalencedGenDem <- function(g, DemandVar, GenerationVar, OutputVar = "BalencedPower"){
  #Balences the generation and demand across multiple isolated componants and removes all dead islands
  #the function takes a graph as an argument and outputs a graph
  #g: the graph of the network will have power balanced
  #DemandVar: The Variable name to be used for calculating nodal demand, character vector
  #GenerationVar: The Variable name to be used for calculating nodal Generation, character vector
  #OutputVar: The name of the variable that will be the new balanced power this is a character string

  df <- dplyr::tibble(
    Demand = igraph::get.vertex.attribute(g, DemandVar) ,
    Generation = igraph::get.vertex.attribute(g, GenerationVar),
    component = igraph::components(g)$membership) %>%
    dplyr::group_by(component) %>%
    #Identify Deadislands and set Generation and Demand to 0
    dplyr::mutate(DeadIsland = sum(Demand)==0 | sum(Generation)==0,
           Demand2 = ifelse(DeadIsland, 0, Demand),
           Generation2 = dplyr::if_else(DeadIsland, 0, Generation)) %>%
    #rebalence remaining componants
    dplyr::mutate(GenBal = ifelse(is.finite(sum(Demand2)/sum(Generation2)),sum(Demand2)/sum(Generation2),1),
           Demand3 = ifelse(GenBal>1,
                            Demand2*(sum(Generation2)/sum(Demand2)), Demand2),
           Generation3 = ifelse(GenBal<1,
                                Generation2*(sum(Demand2)/sum(Generation2)), Generation2),
           Output = Generation3 - Demand3
    ) %>%
    dplyr::ungroup

  g2 <- igraph::set.vertex.attribute(g, OutputVar, value =df$Output )
  g2 <- igraph::delete.vertices(g2, df$DeadIsland)
  return(g2)


}
