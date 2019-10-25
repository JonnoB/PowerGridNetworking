#' Node pairs
#'
#' This function finds every pair of nodes in the network. It takes in a igraph power network and outputs a dataframe.
#' The dataframe contains the node pair names, their node ID number type of node generation/demand/Transmission, the Net generation of
#' the node, and wether the node pair is generation and demand
#'
#' @param g An igraph object of a power network.
#' @param node_name Is the vertex attribute that contains the node names.
#' @param generation The vertex attribute containing the node generation data.
#' @param  demand The vertec attribute containing the node generation data.
#' @param ptdf An optional arguement. The PDTF matrix as produced by imp_ptdf. default is NULL. If not ptdf is supplied one is calculated.
#' @param bus_order Only used if no PDF is supplied. The vertex attribute that contains the rank order for slack reference.
#' @export
#' @seealso \code{\link{slack_ref_func}}
#'
node_pairs <- function(g, node_name = "name", generation = "generation", demand = "demand", ptdf = NULL, bus_order = "bus_order"){
  if(is.null(ptdf)){
    print("Creating PTDF")
    slack_ref <-slack_ref_func(g, node_name, bus_order)
    ptdf <- imp_ptdf(g, slack_ref$node_name)$ptdf
  }
  #Describe the node type
  GenAndDem <-data_frame(
    node_name = get.vertex.attribute(g, node_name),
    type = case_when(
      get.vertex.attribute(g, demand) > get.vertex.attribute(g, generation) ~ "demand",
      get.vertex.attribute(g, demand) < get.vertex.attribute(g, generation) ~ "generation",
      TRUE ~"Transmission"
    ),
    NetGen = get.vertex.attribute(g, generation)-get.vertex.attribute(g, demand))
  #Find every combination of demand and generation pair there should be sum(GenAndDem$type=="generation") * sum(GenAndDem$type=="demand") of them
  #Unless the slack node is a demand or generation node then it will be off slightly!
  Combos <- combn(1:ncol(ptdf), 2) %>%
    t %>%
    as.data.frame() %>%
    mutate(node_name1 = colnames(ptdf)[V1],
           node_name2 = colnames(ptdf)[V2],
           type1 = GenAndDem$type[match(node_name1,GenAndDem$node_name)],
           type2 = GenAndDem$type[match(node_name2,GenAndDem$node_name)],
           NetGen1 =  GenAndDem$NetGen[match(node_name1,GenAndDem$node_name)],
           NetGen2 =  GenAndDem$NetGen[match(node_name2,GenAndDem$node_name)],
           GenPair = ifelse((type1=="generation" & type2 == "demand") |(type2=="generation" & type1 == "demand"), TRUE, FALSE))
  return(Combos)
}
