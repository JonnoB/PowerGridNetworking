#' Find components of a graph that have changed
#' Find components of a graph that have changed
#'
#' Using this should be able to cut down on the amount of calculation done on the power flow equations
#' This function checks to see which subcomponents have changed in the graph and which are the same.
#' It does this by comparing edges. This is because a component may have had an edge removed but still be intact.
#' However, the powerflow will still need to be re-calculated.
#' The function outputs a logical value
#' @param g An igraph object. The current graph
#' @param g_0 An igraph object. the previous graph
#' @param edge_name a character string. The graph attribute describing the edge node_name, default is set to 'node_name'
#' @export
#'
components_differ<-function(g, g_0, edge_name = "name"){
  #This function checks to see which subcomponents have changed in the graph and which are the same.
  # It does this by comparing edges. This is because a component may have had an edge removed but still be intact.
  # However, the powerflow will still need to be re-calculated.
  #using this should be able to cut down on the amount of calculation done on the power flow equations
  #g the current graph
  #g_0 the previous graph
  gcomps <- components(g)$no #current graph
  g0comps <- components(g_0)$no #previous graph
if(gcomps==0){
  #This is necessary as in rare occasions a grid collapses meaning there are no Components
  #in such cases the function throws an error. This if statement prevents that
  #Instead returning a 0 meaning the graphs are different which they are.
  dat <- 0
} else{
    #Create a list of the nodes names in eaah component, for the current graph
    gcompsList <- 1:gcomps %>%
      map(~ {
        newgraph <- delete.vertices(g, components(g)$membership != .x)
        get.edge.attribute(newgraph, edge_name)}
      )
    #Create a list of the nodes names in each component, for the previous
    g0compsList <- 1:g0comps %>%
      map(~ {
        newgraph <- delete.vertices(g_0, components(g_0)$membership != .x)
        get.edge.attribute(newgraph, edge_name)}
      )
    #Make a matrix that allows each list to be checked against the other
    dat <- expand.grid(1:gcomps, 1:g0comps)
   #The check for a single node prevents empty vectors providing false positives
    dat <- map2_lgl(.x = dat$Var1, .y  = dat$Var2, ~ {
      base::all(g0compsList[[.y]] %in% gcompsList[[.x]] #all nodes in component are equal
                & length(g0compsList[[.y]])>0) #and the component is more than a single node
    }
    ) %>%
      matrix(data = . , nrow = gcomps) %>%
      colSums
  }
  dat == 0
}
