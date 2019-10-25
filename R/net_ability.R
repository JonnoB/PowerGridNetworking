#' Net-ability
#'
#' This function calculates the net-ability of a graph.
#'
#' The algorithm was implemented from "power grids vulnerability a complex networks approach" Arianos 2009 and
#' "Analysis of structural vulnerabilities in power transmission grids" Bompard 2009.
#' \code{g_orig} is an important vairable if your network has already been attacked. If generation or demand nodes are removed
#'  from the graph due to failure they will not be considered in the net-ability potentially increasing new graphs score as
#'  the total number of node pairs will be smaller in the newgraph. Because of this the number of node pairs always has to be
#'  calculated from the original graph
#'
#' @param g The graph for which net-ability will be calculated
#' @param g_orig The original graph structure. The defualt is the g
#' @export
#' @examples
#' NetAbScore <-net_ability(g)
net_ability <- function(g, g_orig = NULL){
  if(is.null(g_orig)){
    g_orig <- g
  }
  #finds the slack reference in each component
  SlackRefCasc <-  tibble(node_name = get.vertex.attribute(g, "name"),
                          Bus.Order = get.vertex.attribute(g, "bus_order"),
                          component = components(g)$membership) %>%
    group_by(component) %>%
    arrange(Bus.Order) %>%
    summarise(node_name = first(node_name),
              nodes = n())
  message(paste("Number of componants", max(SlackRefCasc$component)))
  #calculates ratio of the electrical distance and the capacity matrix
    Numerator <- 1:nrow(SlackRefCasc) %>%
    map(~{
          slack_ref <- SlackRefCasc %>% slice(.x)
          message("Component", .x)
          gsubset <- delete.vertices(g, components(g)$membership != .x)
          lemma <- imp_ptdf(gsubset, slack_ref$node_name)
          message("Calculating the electrical distance") #these two can take a long time so a message is given
          ElecDist <- electrical_distance(lemma$imp)
          message("Calculating the Capacity Matrix")
          CapacityMatrix <- transfer_capacity(gsubset, lemma$ptdf)
          Numerator <- CapacityMatrix/ElecDist
          Numerator[!is.finite(Numerator)] <- 0
          Numerator <-sum(Numerator)
          return(Numerator)
    }) %>%
    unlist %>%
    sum
  #gets the counts of the number of generator and load busses
  #This keeps the slack variable in. That may be not a good idea I am not sure
  GenAndDem <-data_frame(
    node_name = get.vertex.attribute(g_orig, "name"),
    type = case_when(
      get.vertex.attribute(g_orig, "demand") > get.vertex.attribute(g_orig, "generation") ~ "demand",
      get.vertex.attribute(g_orig, "demand") < get.vertex.attribute(g_orig, "generation") ~ "generation",
      TRUE ~"Transmission"
    )) %>%
    group_by(type) %>%
    summarise(counts = n()) %>%
    filter(type != "Transmission")
  Denominator <- prod(GenAndDem$counts)
  NetAb <- sum(Numerator)/Denominator
  return(NetAb)
}
