#' Find which edges are over the limit
#'
#'This function checks to see which lines are over limit and removes them and re-calculates power flow in the network
#'
#'The function uses a node attribute "Bus.Order" to identify the slack bus. This may be changed in later versions
#'
#' @param g An igraph object that represents a power-grid
#' @export

CalcOverLimit <- function(g){
  #This function calculates which edges are over the limit for the current network configuration and demand
  #production profile.
  #g: a graph with multiple edge and vertex attributes. The graph is assumed to be balenced in demand and production.

  #finds the slack reference in each component
  SlackRefCasc <-  tibble(name = get.vertex.attribute(g, "name"),
                          Bus.Order = get.vertex.attribute(g, "Bus.Order"),
                          component = components(g)$membership) %>%
    group_by(component) %>%
    arrange(Bus.Order) %>%
    summarise(name = first(name),
              Nodes = n())

  #Calculate power flow for each component of the network as seperate networks
  gList <- 1:nrow(SlackRefCasc) %>%
    map(~{

      #print(paste("PowerFlow for componant", .x))

      SlackRef <- SlackRefCasc %>% slice(.x)

      gsubset <- delete.vertices(g, components(g)$membership != .x)

      if(SlackRef$Nodes > 1){

        gsubset <- PowerFlow(gsubset, SlackRef$name)
      }

      gsubset

    })

  gOut <- gList %>%
   Reduce(union2, .)


  return(gOut)

}
