#' Net-ability
#'
#' This function calculates the net-ability of a graph.
#'
#' The algorithm was implemented from "power grids vulnerability a complex networks approach" Arianos 2009 and
#' "Analysis of structural vulnerabilities in power transmission grids" Bompard 2009.
#' \code{gOrig} is an important vairable if your network has already been attacked. If Generation or demand nodes are removed
#'  from the graph due to failure they will not be considered in the net-ability potentially increasing new graphs score as
#'  the total number of node pairs will be smaller in the newgraph. Because of this the number of node pairs always has to be
#'  calculated from the original graph
#'
#' @param g The graph for which net-ability will be calculated
#' @param gOrig The original graph structure. The defualt is the g
#' @export
#' @examples
#' NetAbScore <-Net_Ability(g)

Net_Ability <- function(g, gOrig = NULL){
  if(is.null(gOrig)){
    gOrig <- g
  }

  #finds the slack reference in each component
  SlackRefCasc <-  dplyr::tibble(name = igraph::get.vertex.attribute(g, "name"),
                          Bus.Order = igraph::get.vertex.attribute(g, "Bus.Order"),
                          component = igraph::components(g)$membership) %>%
    dplyr::group_by(component) %>%
    dplyr::arrange(Bus.Order) %>%
    dplyr::summarise(name = dplyr::first(name),
              Nodes = dplyr::n())

  message(paste("Number of componants", max(SlackRefCasc$component)))

  #calculates ratio of the electrical distance and the capacity matrix
    Numerator <- 1:nrow(SlackRefCasc) %>%
    purrr::map(~{
          SlackRef <- SlackRefCasc %>% dplyr::slice(.x)
          message("Component", .x)

          gsubset <- igraph::delete.vertices(g, igraph::components(g)$membership != .x)

          lemma <- ImpPTDF(gsubset, SlackRef$name)

          message("Calculating the electrical distance") #these two can take a long time so a message is given

          ElecDist <- ElectricalDistance(lemma$Imp)

          message("Calculating the Capacity Matrix")

          CapacityMatrix <- TransferCapacity(gsubset, lemma$PTDF)

          Numerator <- CapacityMatrix/ElecDist

          Numerator[!is.finite(Numerator)] <- 0

          Numerator <-sum(Numerator)

          return(Numerator)

    }) %>%
    unlist %>%
    sum

  #gets the counts of the number of generator and load busses
  #This keeps the slack variable in. That may be not a good idea I am not sure
  GenAndDem <-dplyr::data_frame(
    name = igraph::get.vertex.attribute(gOrig, "name"),
    type = dplyr::case_when(
      igraph::get.vertex.attribute(gOrig, "Demand") > igraph::get.vertex.attribute(gOrig, "Generation") ~ "Demand",
      igraph::get.vertex.attribute(gOrig, "Demand") < igraph::get.vertex.attribute(gOrig, "Generation") ~ "Generation",
      TRUE ~"Transmission"
    )) %>%
    dplyr::group_by(type) %>%
    dplyr::summarise(counts = dplyr::n()) %>%
    dplyr::filter(type != "Transmission")

  Denominator <- prod(GenAndDem$counts)

  NetAb <- sum(Numerator)/Denominator

  return(NetAb)
}
