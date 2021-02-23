#' Calculate the effects of a cascade
#'
#' Iterates through the network removing edges until no edges are over the line limit and the network has stabilised.
#'
#' This version of cascade works with the attack_the_grid function. It is confusing as it is called cascade2 not one but there we go.
#'
#' The function outputs a list of 3 elements. the first element is the updated igraph oobject after the cascade has terminated.
#' The second element is a vector of node power values, the third element is a vector of the edge status.
#'
#' @param g An igraph object. The graph at its current state in the attack process
#' @param g0 An igraph object. An optional value that defines the reference network
#' @param node_power A vector of the current node power for this round
#' @param edge_status A vector of current edge status, this is an integer vector of 4 levels 0-3
#' @param AZero A matrix. THe transmission matrix of the network
#' @param LineProperties The diagonal Line properties matrix
#' @param Demand the name of the node Load variable. A character string.
#' @param Generation The name of the node generation variable. A character string.
#' @param EdgeName The variable that holds the edge names, a character string.
#' @param VertexName The variable that holds the names of the nodes, to identify the slack ref. a character string
#' @param Net_generation The name that the net generation data for each node is held in
#' @param power_flow A character string. This value indicates the name of the edge attribute that holds power flow, the default is "PowerFlow"
#' @param edge_limit A character string. This value indicates the name of the edge attribute that holds the edge limit, the default is "Link.Limit"
#' @export
#' @seealso \code{\link{attack_the_grid}}


#Version of cacade that outputs a list of vectors and the updated graph
Cascade2 <- function(g,
                     g0 = NULL,
                     node_power = node_power,
                     edge_status = edge_status,
                     AZero,
                     LineProperties,
                     Generation = "Generation",
                     Demand = "Demand",
                     EdgeName = "Link",
                     VertexName = "name",
                     Net_generation = "BalencedPower",
                     power_flow = "PowerFlow",
                     edge_limit = "Link.Limit"
){
  #This Function iterates through the network removing edges until there are no further overpower edges to remove
  #This function uses the Bus order to choose the slack reference should this be changed?
  #Iteration: the number of iteration number of the cascade, used to keep track of what is going on
  CascadeContinues <- TRUE
  Iteration <- 0
  StopCascade <- Inf

  node_names <-names(node_power)
  edge_names <- names(edge_status)

  while(CascadeContinues & Iteration != StopCascade){

    Iteration <- Iteration + 1

    comp_info <- igraph::components(g)

    #if graph comparison is being used then the process starts here.
    #This stops needless subgraphs being recalculated then joined.
    #This cannot be the first iteration and there needs to be more than one component
    if(!is.null(g0) & comp_info$no>1){
      #find components that need to be recalculuated
      RecalcFlow <- Components_differ(g, g0, EdgeName = EdgeName)
      #save the full graph
      g_temp <- g
      #create a subgraph of elements that do not need to be recalculated
      g <- igraph::delete.vertices(g,( 1:igraph::vcount(g))[!(comp_info$membership %in% RecalcFlow)])
    # print(paste("update",igraph::components(g)$no, "of", igraph::components(g_temp)$no))
    }

    #calculate the power flow on the subgraph
    g <- PowerFlow(g, AZero, LineProperties,
                                EdgeName = EdgeName,
                                VertexName = VertexName,
                                Net_generation = Net_generation,
                                power_flow = power_flow)

    if(!is.null(g0)& comp_info$no>1){
      #add back in power flow to the full graph
      changed_edge_index <- match(igraph::edge_attr(g, EdgeName), igraph::edge_attr(g_temp, EdgeName) )

      g <- igraph::set_edge_attr(g_temp, name = power_flow, index =  changed_edge_index, value = igraph::edge_attr(g, power_flow) )

    }
    ####
    #
    #Delete Edges that are over the Limit
    #
    ####

    edge_index_over <- (1:igraph::ecount(g))[abs(igraph::edge_attr(g, name = power_flow)) > igraph::edge_attr(g, name = edge_limit)]
    #if no edges are over the limit setting the DeleteEdges to NA prevents errors being thrown
    Overloads <- igraph::edge_attr(g, name = EdgeName, index = edge_index_over)

    #Replace the NA value in the vector with the loss through overloading
    #Errors may occur here due to matric vector conversion.
    edge_status[edge_names %in% Overloads] <- 3L

    #delete the overlaoded edges from the network
    g2<- igraph::delete.edges(g, edge_index_over)

    #Balence grid after over powered lines and edges are removed
    g3 <- BalencedGenDem(g2, Demand, Generation, OutputVar = Net_generation)

    ####
    #
    #lost through islanding
    #The previous value are modified in place, tiny speed and memory saving. g4 could actually overwrite g
    #
    ####
    #Edges in original network and g2
    edge_orig_and_g <- edge_names %in% igraph::edge_attr(g2, name = EdgeName)
    #Edges in original network but and g2
    edge_orig_and_g2 <- edge_names %in% igraph::edge_attr(g3, name = EdgeName)

    #Nodes in original network and g2
    nodes_orig_and_g <- node_names %in% igraph::vertex_attr(g2, name = VertexName)
    #Nodes in original network and g3
    nodes_orig_and_g2 <- node_names %in% igraph::vertex_attr(g3, name = VertexName)

    #The difference between g2 and g3
    edge_diff <- edge_orig_and_g != edge_orig_and_g2
    node_diff <- nodes_orig_and_g != nodes_orig_and_g2
    #Replace the NA value in the matrix with the loss through islanding code
    #In the case all is FALSE an error is thrown. This if statement avoids the update in that case
    if(!all(edge_diff==FALSE)){
    edge_status[edge_diff] <- 2L
    }
    if(!all(node_diff==FALSE)){
      node_power[node_diff ] <- +Inf
    }



    #Checks to see if there are any changes in the edges of the network.
    #As no new edges can appear if the number of edges in the two graphs is identical then
    #the graph structures must be identical. Sole vertices are irrelevant for the purposes of a cascade
    #In additiona any lone vertices lacking power or demand would have already been removed.
    #If all the edges are the same returns TRUE
    edgesequal <-igraph::ecount(g2) == igraph::ecount(g)

    #Terminates the cascade if there are no edges left preventing errors.
    GridCollapsed<- igraph::ecount(g2)==0
    #Checking there are edges left prevents trying to find a component in the Slackref and throwing an error.
    CascadeContinues <- !isTRUE(edgesequal) & !GridCollapsed

    #Cascade2 avoides the issues with the list version of cascade be cause it doesn't matter if
    #the  matrix is updated again the system stays identical.
    #This is why this function never had the update issue of the other one had.
    g0 <- g
    g <- g3

  }
  #message(paste("Cascade has completed with", Iteration, "iterations"))

  #Outputs the list which will be fed back into the Attack the grid function
  Out <- list(g = g, node_power = node_power, edge_status = edge_status)

  return(Out)

}
