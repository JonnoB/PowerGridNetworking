#' Calculate DC power flow
#'
#' TCalculates the PowerFlow from a graph that contains the following attributes,
#' named edges, edgeweights, a balanced power generation and demand column, Powerflow (will be overwritten)
#' @param g an igraph object
#' @param SlackRef the node to remove from the calculations to prevent the matrix being singular
#' @param EdgeName the variable that holds the edge names, a character string.
#' @param VertexName the variable that holds the names of the nodes, to identify the slack ref. a character string
#' @param Net_generation the name that the net generation data for each node is held in
#' @keywords power flow
#' @export
#' @examples
#' PowerFlow(g Slackref)

PowerFlow <- function(g, SlackRef, EdgeName = "Link", VertexName = "name", Net_generation = "BalencedPower"){
  #Calculates the PowerFlow from a graph that contains the following attributes
  #named edges, edgeweights, a balanced power generation and demand column, Powerflow (will be overwritten)
  #g: an igraph object
  #SlackRef: the node to remove from the calculations to prevent the matrix being singular

  InjectionVector <- get.vertex.attribute(g, name = Net_generation)[get.vertex.attribute(g, name = VertexName)!=SlackRef]

  Power <- ImpPTDF(g, SlackRef, EdgeName, VertexName)$PTDF %*% InjectionVector

  g <- set_edge_attr(g, "PowerFlow", value = Power)

  return(g)
}
