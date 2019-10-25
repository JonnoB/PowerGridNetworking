#' Calculate the effects of a cascade
#'
#' Iterates through the network removing edges until no edges are over the line limit and the network has stabilised.
#'
#' This is a recursive function that calls itself repeatedly until the cascade has come to a stop and the network has stabilised.
#'   The function produces a list of graphs showing each state of the cascade. This function is mostly used as part of the
#'   attack_the_grid function.
#' @param network_list A list of power-grid networks.
#' @param iteration The current iteration number
#' @param stop_cascade The number of iterations when cascade will be forced to terminate. An integer, default is set to infinity.
#' @param demand the node_name of the node Load variable. A character string.
#' @param generation the node_name of the node generation variable. A character string.
#' @param edge_name the variable that holds the edge names, a character string.
#' @param node_name the variable that holds the names of the nodes, to identify the slack ref. a character string
#' @param net_generation the node_name that the net generation data for each node is held in
#' @export
#' @seealso \code{\link{attack_the_grid}}
cascade <- function(network_list,
                    iteration = 0,
                    stop_cascade = Inf,
                    g_0 = NULL,
                    generation = "generation",
                    demand = "demand",
                    edge_name = "edge_name",
                    node_name = "name",
                    net_generation = "net_generation"){
  #This Function iterates through the network removing edges until there are no further overpower edges to remove
  #This function uses the Bus order to choose the slack reference should this be changed?
  #iteration: the number of iteration number of the cascade, used to keep track of what is going on
  g <- network_list[[length(network_list)]]
  iteration <- iteration + 1
  #message(paste("Using previous graph", !is.null(g_0)))
  #if graph comparison is being used then the process starts here.
  #This stops needless subgraphs being recalculated then joined.
  #The rejoining process is slow so this speeds it up
  if(!is.null(g_0)){
    #find components that need to be recalculuated
    RecalcFlow <- (1:components(g)$no)[components_differ(g, g_0, edge_name = edge_name)]
    #create a subgraph of elements that do not need to be recalculated
    gNochange <- delete.vertices(g, (1:vcount(g))[components(g)$membership %in% RecalcFlow])
    #create a subgraph of parts that do need to be recalculated
    g <- delete.vertices(g,( 1:vcount(g))[!(components(g)$membership %in% RecalcFlow)])
    #  message(paste("components changed since previous", paste(RecalcFlow, collapse = ",")))
  }
  g <- calc_over_limit(g,  edge_name, node_name, net_generation)
  if(!is.null(g_0)){
    #print("Joining pre and post") #These joins are fast now and don't need to be mentioned
    g <- union_2(gNochange, g)
    #print("Join complete")
  }
  #Delete Edges that are over the Limit
  DeleteEdges <- as_data_frame(g) %>%
    mutate(index = 1:n(),
           Over.Limit = abs(power_flow) > edge_limit) %>%
    filter(Over.Limit)
  #print("OVERERLOADSSS")
  #If the cascade has been going for more than 1 round then the overloaded edges need to be added together
  if(iteration==1){
    Overloads <- DeleteEdges %>% pull(edge_name)
  } else {
    Overloads <- c(graph_attr(g, "EdgesOverloaded" ), DeleteEdges %>% pull(edge_name))
    #Prints names of overloaded edges
    #print("Overloads")
    #print(Overloads)
  }
  #write vector of overloaded edges
  g <- set_graph_attr(g, "EdgesOverloaded", Overloads)
  #g is structurally changed here and becomes g_2
  g_2 <- delete.edges(g, DeleteEdges$index)
  #Balence grid after over powered lines and edges are removed
  g_2 <- balance_generation_and_demand(g_2, demand, generation, output_var = net_generation)
  #Checks to see if there are any changes in the edges of the network by component.
  #If all the edges are the same returns TRUE
  edgesequal <- all(!components_differ(g_2, g, edge_name = edge_name))
  #Terminates the cascade if there are no edges left preventing errors.
  GridCollapsed<- ecount(g_2)==0
  #Checking there are edges left prevents trying to find a component in the Slackref and throwing an error.
  CascadeContinues <- !isTRUE(edgesequal) & !GridCollapsed
  if(CascadeContinues & iteration != stop_cascade){
    #add the new network into the list
    network_list <- c(network_list, list(g_2))
    #update the list with the new lists created in the cascade
    network_list <- cascade(network_list,
                           iteration = iteration,
                           stop_cascade = stop_cascade,
                           g_0 = g,
                           generation = generation,
                           demand = demand,
                           edge_name = edge_name,
                           node_name = node_name ,
                           net_generation = net_generation)
  }
  message(paste("Cascade has completed with", iteration, "iterations"))
  return(network_list)
}
