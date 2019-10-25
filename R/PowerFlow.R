#' Calculate DC power flow
#'
#' Calculates the PowerFlow from a graph that contains the following attributes,
#' named edges, edgeweights, a balanced power generation and demand column. Writes a new edge attribute of power flow
#' (existing attribute of the same name will be overwritten). 
#' The function outputs a a graph with the correct power flow values
#' @param g An igraph object representing the power grid
#' @param SlackRef A character strong. The node to remove from the calculations to prevent the matrix being singular
#' @param EdgeName The variable that holds the edge names, a character string.
#' @param VertexName The variable that holds the names of the nodes, to identify the slack ref. a character string
#' @param Net_generation The name that the net generation data for each node is held in
#' @param power_flow A character string. The name of the edge attribute that will hold the power flow information
#' @keywords power flow
#' @export
#' @examples
#' PowerFlow(g Slackref)

PowerFlow <- function(g, SlackRef, EdgeName = "Link", VertexName = "name", Net_generation = "BalencedPower", power_flow = "PowerFlow"){
  #Calculates the PowerFlow from a graph that contains the following attributes
  #named edges, edgeweights, a balanced power generation and demand column, Powerflow (will be overwritten)
  #g: an igraph object
  #SlackRef: the node to remove from the calculations to prevent the matrix being singular

  InjectionVector <- get.vertex.attribute(g, name = Net_generation)[get.vertex.attribute(g, name = VertexName)!=SlackRef]

  Power <- ImpPTDF(g,  SlackRef, EdgeName, VertexName)$PTDF %*% InjectionVector

  g <- set_edge_attr(g, name = power_flow, value = Power)

  return(g)
}
