#' Electrical Edge Centrality
#' 
#' Calculates the electrical edge centrality of the graph
#' 
#' @param g An igraph object of a power network.
#' @param Node_name Is the vertex attribute that contains the node names.
#' @param Generation The vertex attribute containing the node generation data.
#' @param  Demand The vertec attribute containing the node generation data.
#' @param Bus_order Only used if no PDF is supplied. The vertex attribute that contains the rank order for slack reference.
#' @export
#' 

ElectricalCentrality <- function(g, Node_name = "name", Generation = "Generation", Demand = "Demand",  Bus_order = "Bus.Order"){
  
  SlackRef<- SlackRefFunc(g, Node_name, Bus_order)
  
  PTDF <- ImpPTDF(g, SlackRef$name)$PTDF
  
  Combos <-NodePairs(g, Node_name = "name", Generation = "Generation", Demand = "Demand", PTDF = PTDF) %>%
    filter(GenPair == TRUE) %>%
    group_by(V1, V2) %>%
    mutate(MinGen = min(abs(NetGen1), abs(NetGen2))) %>%
    ungroup
  
  
  #This has to go through all Gen/Dem pairs and so can take a moment
  print("Calculating Injection vector for all node pairs")
  EdgeVect <- rep(0,ncol(PTDF))
  #Each 0 vector has the minimum maximum value of the Gen/Dem Pair
  InjectionVector <- (1:nrow(Combos)) %>% 
    map(~{
      
      EdgeVect[Combos$V1[.x]] <- if_else(Combos$NetGen1[.x]>0, Combos$MinGen[.x], -Combos$MinGen[.x])
      EdgeVect[Combos$V2[.x]] <- if_else(Combos$NetGen2[.x]>0, Combos$MinGen[.x], -Combos$MinGen[.x])
      
      data_frame(EdgeVect) %>%
        set_names(paste0("Combo", .x))
      
    }) %>% bind_cols() %>%
    rowSums()
  
  
  ElectricalCentrality <- (PTDF %*% InjectionVector) %>% abs
  
  ElectricalCentrality <- as_tibble(ElectricalCentrality) %>%
    set_names("ElectricalCentrality") %>% 
    mutate(Edgename = get.edge.attribute(g, "name"))
  
  return(ElectricalCentrality)
}