#' Calculate power flow across all components in the network
#'
#' This function calculates the power flow across all components of the network, then re combines them back into a single network using union2
#' This function has a legacy name. It does not calculate which edges are over the limit. This will be changed at some point
#'
#' The function uses SlackRefFunc and the largest generator to find the slack bus
#'
#' @param g An igraph object that represents a power-grid
#' @param EdgeName The variable that holds the edge names, a character string.
#' @param VertexName The variable that holds the names of the nodes, to identify the slack ref. a character string
#' @param Net_generation The name that the net generation data for each node is held in
#' @param power_flow  A character string. This value indicates the name of the edge attribute that holds power flow, the default is "PowerFlow"
#' @export
#' @seealso  \code{\link{SlackRefFunc}},\code{\link{union2}}
#' @examples 
#' CalcOverLimit(g, EdgeName = "Link", VertexName = "name", Net_generation = "BalencedPower")

CalcOverLimit <- function(g, EdgeName = "Link", VertexName = "name", Net_generation = "BalencedPower", power_flow = "PowerFlow"){

  #g: a graph with multiple edge and vertex attributes. The graph is assumed to be balenced in demand and production.

  #finds the slack reference in each component
  SlackRefCasc <-  SlackRefFunc(g, VertexName, Generation = Net_generation)

  #message(paste("Total network components ", nrow(SlackRefCasc))) #Not sure how useful this is anymore


  #Sometimes the Slackref df has 0 rows throwing an errors. so this code prevents that
  if(nrow(SlackRefCasc)!=0){
  #Calculate power flow for each component of the network as seperate networks
  gList <- 1:nrow(SlackRefCasc) %>%
    map(~{

      #print(paste("PowerFlow for componant", .x))

      SlackRef <- SlackRefCasc %>% slice(.x)

      gsubset <- delete.vertices(g, components(g)$membership != .x)

      if(SlackRef$Nodes > 1){

        gsubset <- PowerFlow(gsubset, SlackRef$name,  EdgeName, VertexName, Net_generation, power_flow)
      }

      gsubset

    })

  gOut <- gList %>%
   Reduce(union2, .)
} else
{
  gOut <- g
}

  return(gOut)

}
