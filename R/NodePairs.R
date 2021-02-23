#' Node pairs
#'
#' This function finds every pair of nodes in the network. It takes in a igraph power network and outputs a dataframe.
#' The dataframe contains the node pair names, their node ID number type of node Generation/Demand/Transmission, the Net generation of
#' the node, and wether the node pair is generation and demand
#'
#' @param g An igraph object of a power network.
#' @param Node_name Is the vertex attribute that contains the node names.
#' @param Generation The vertex attribute containing the node generation data.
#' @param  Demand The vertec attribute containing the node generation data.
#' @param PTDF An optional arguement. The PDTF matrix as produced by ImpPTDF. default is NULL. If not PTDF is supplied one is calculated.
#' @param Bus_order Only used if no PDF is supplied. The vertex attribute that contains the rank order for slack reference.
#' @export
#' @seealso \code{\link{SlackRefFunc}}
#'

NodePairs <- function(g, Node_name = "name", Generation = "Generation", Demand = "Demand", PTDF = NULL, Bus_order = "Bus.Order"){

  if(is.null(PTDF)){
    print("Creating PTDF")
    SlackRef <-SlackRefFunc(g, Node_name, Bus_order)
    PTDF <- ImpPTDF(g, SlackRef$name)$PTDF
  }

  #Describe the node type
  GenAndDem <-dplyr::data_frame(
    name = igraph::get.vertex.attribute(g, Node_name),
    type = dplyr::case_when(
      igraph::get.vertex.attribute(g, Demand) > igraph::get.vertex.attribute(g, Generation) ~ "Demand",
      igraph::get.vertex.attribute(g, Demand) < igraph::get.vertex.attribute(g, Generation) ~ "Generation",
      TRUE ~"Transmission"
    ),
    NetGen = igraph::get.vertex.attribute(g, Generation)-igraph::get.vertex.attribute(g, Demand))


  #Find every combination of Demand and Generation pair there should be sum(GenAndDem$type=="Generation") * sum(GenAndDem$type=="Demand") of them
  #Unless the slack node is a demand or generation node then it will be off slightly!
  Combos <- combn(1:ncol(PTDF), 2) %>%
    t %>%
    as.data.frame() %>%
    dplyr::mutate(name1 = colnames(PTDF)[V1],
           name2 = colnames(PTDF)[V2],
           type1 = GenAndDem$type[match(name1,GenAndDem$name)],
           type2 = GenAndDem$type[match(name2,GenAndDem$name)],
           NetGen1 =  GenAndDem$NetGen[match(name1,GenAndDem$name)],
           NetGen2 =  GenAndDem$NetGen[match(name2,GenAndDem$name)],
           GenPair = ifelse((type1=="Generation" & type2 == "Demand") |(type2=="Generation" & type1 == "Demand"), TRUE, FALSE))

  return(Combos)

}
