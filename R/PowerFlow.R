#' Calculate DC power flow
#'
#' Calculates the PowerFlow from a graph that contains the following attributes,
#' named edges, edgeweights, a balanced power generation and demand column. Writes a new edge attribute of power flow
#' (existing attribute of the same name will be overwritten). 
#' The function outputs a a graph with the correct power flow values
#' @param g An igraph object representing the power grid
#' @param AZero A numeric matrix The transmission matrix of the original network
#' @param LineProperties A numeric matrix. a diagonal matrix of the Y characteristic of the network
#' @param EdgeName The variable that holds the edge names, a character string.
#' @param VertexName The variable that holds the names of the nodes, to identify the slack ref. a character string
#' @param Net_generation The name that the net generation data for each node is held in
#' @param power_flow A character string. The name of the edge attribute that will hold the power flow information
#' @keywords power flow
#' @export
#' @examples
#' PowerFlow(g Slackref)

#Azero is calculated externally
PowerFlow <- function(g, AZero, LineProperties, 
                       EdgeName = "Link", 
                       VertexName = "name", 
                       Net_generation = "BalencedPower", 
                       power_flow = "PowerFlow"){

  #generate the slack reference bus for each component of the network
  #This creates much smaller matrices which are quicker to invert and operate on.
  slack_ref_df <-  SlackRefFunc(g, VertexName, Generation = Net_generation)
  
  if(nrow(slack_ref_df)!=0){
    #Calculate power flow for each component of the network as seperate networks
    gList <- 1:nrow(slack_ref_df) %>%
      map(~{
        
        #print(paste("PowerFlow for componant", .x))
        
        SlackRef <- slack_ref_df %>% slice(.x)
        
        gsubset <- delete.vertices(g, components(g)$membership != .x)
        
        if(SlackRef$Nodes > 1){
          
          #gsubset <- PowerFlow2(gsubset, SlackRef$name, AZero = AZero, LineProperties = LineProperties, EdgeName, VertexName, Net_generation, power_flow)
          
          InjectionVector <- get.vertex.attribute(gsubset, name = Net_generation)[get.vertex.attribute(gsubset, name = VertexName)!=SlackRef$name]
          
          Power <- ImpPTDF(gsubset,  
                            SlackRef$name, 
                            AZero = AZero, 
                            LineProperties = LineProperties, 
                            EdgeName, 
                            VertexName, 
                            PTDF_only = TRUE)$PTDF %*% InjectionVector
          
          gsubset <- set_edge_attr(gsubset, name = power_flow, value = Power)
        }
        
        gsubset
        
      })
    #This function could be replaced with the method that just matches edges but it is so fast I don't care enough
    #It would mean that union could be completley removed which woul be more secure and simpler
    g <- gList %>%
      Reduce(union2, .)
  }

  return(g)
}
