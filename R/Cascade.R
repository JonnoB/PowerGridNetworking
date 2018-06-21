#' Calculate the effects of a cascade
#'
#' Iterates through the network removing edges until no edges are over the line limit and the network has stabilised.
#'
#' This is a recursive function that calls itself repeatedly until the cascade has come to a stop and the network has stabilised.
#'   The function produces a list of graphs showing each state of the cascade. This function is mostly used as part of the
#'   AttackTheGrid function.
#' @param NetworkList A list of power-grid networks.
#' @param Iteration The current iteration number
#' @param StopCascade The number of iterations when cascade will be forced to terminate. An integer, default is set to infinity.
#' @export
#' @seealso \code{\link{AttackTheGrid}}

############

#Remove the cascade2 from the recurseive process at the bottom!


##########

Cascade <- function(NetworkList, Iteration = 0, StopCascade = Inf, g0 = NULL){
  #This Function iterates through the network removing edges until there are no further overpower edges to remove
  #This function uses the Bus order to choose the slack reference should this be changed?
  #Iteration: the number of iteration number of the cascade, used to keep track of what is going on

  g <- NetworkList[[length(NetworkList)]]

  Iteration <- Iteration + 1
  #message(paste("Using previous graph", !is.null(g0)))

  #if graph comparison is being used then the process starts here.
  #This stops needless subgraphs being recalculated then joined.
  #The rejoining process is slow so this speeds it up
  if(!is.null(g0)){
    #find components that need to be recalculuated
    RecalcFlow <- (1:components(g)$no)[Components_differ(g, g0)]
    #create a subgraph of elements that do not need to be recalculated
    gNochange <- delete.vertices(g, (1:vcount(g))[components(g)$membership %in% RecalcFlow])
    #create a subgraph of parts that do need to be recalculated
    g <- delete.vertices(g,( 1:vcount(g))[!(components(g)$membership %in% RecalcFlow)])
  #  message(paste("components changed since previous", paste(RecalcFlow, collapse = ",")))
  }


  g <- CalcOverLimit(g)


  if(!is.null(g0)){
    #print("Joining pre and post") #These joins are fast now and don't need to be mentioned
    g <- union2(gNochange, g)
    #print("Join complete")
  }

  #Delete Edges that are over the Limit


  DeleteEdges <- as_data_frame(g) %>%
    mutate(index = 1:n(),
           Over.Limit = abs(PowerFlow) > Link.Limit) %>%
    filter(Over.Limit)

  #g is structurally changed here and becomes g2
  g2 <- delete.edges(g, DeleteEdges$index)

  #Balence grid after over powered lines and edges are removed
  g2 <- BalencedGenDem(g2, "Demand", "Generation")

  #Checks to see if there are any changes in the edges of the network by component.
  #If all the edges are the same returns TRUE
  edgesequal <- all(!Components_differ(g2,g))

  #Terminates the cascade if there are no edges left preventing errors.
  GridCollapsed<- ecount(g2)==0
  #Checking there are edges left prevents trying to find a component in the Slackref and throwing an error.
  CascadeContinues <- !isTRUE(edgesequal) & !GridCollapsed

  if(CascadeContinues & Iteration != StopCascade){
    #add the new network into the list
    NetworkList <- c(NetworkList, list(g2))
    #update the list with the new lists created in the cascade
    NetworkList <- Cascade(NetworkList, Iteration, StopCascade, g0 = g)
  }

  message(paste("Cascade has completed with", Iteration, "iterations"))

  return(NetworkList)

}
