#' Calculate DC power flow
#'
#' TCalculates the PowerFlow from a graph that contains the following attributes,
#' named edges, edgeweights, a balanced power generation and demand column, Powerflow (will be overwritten)
#' @param g an igraph object
#' @param SlackRef the node to remove from the calculations to prevent the matrix being singular
#' @keywords power flow
#' @export
#' @examples
#' PowerFlow(g Slackref)

PowerFlow <- function(g, SlackRef){
  #Calculates the PowerFlow from a graph that contains the following attributes
  #named edges, edgeweights, a balanced power generation and demand column, Powerflow (will be overwritten)
  #g: an igraph object
  #SlackRef: the node to remove from the calculations to prevent the matrix being singular

  InjectionVector <- get.vertex.attribute(g, "BalencedPower")[get.vertex.attribute(g, "name")!=SlackRef]

  Power <- ImpPTDF(g, SlackRef)$PTDF %*% InjectionVector

  g <- set_edge_attr(g, "PowerFlow", value = Power)

  return(g)
}
