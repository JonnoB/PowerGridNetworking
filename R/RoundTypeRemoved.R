#' Round Removed and Type of Removal
#' 
#' This function finds the round removed of all Nodes and Edges in a network as well as whether the removal type was Targeted/Overloaded/Islanded
#' 
#' The function accepts the list of lists produced by the function attack the grid and outputs a 
#'     list containing two dataframes for Nodes and Edges respectively.
#' 
#' @param AttackSeries The list of lists produced by attack the grid
#' @export

RoundTypeRemoved <- function(AttackSeries){
  
  #finds the round removed for each type
  TempNodes <- AttackSeries %>% RoundRemoved(type = "Node")
  TempEdges <- AttackSeries %>% RoundRemoved(type = "Edge")
  
  #gets the original graph
  baseg <- AttackSeries[[1]][[1]]
  
  #Join edges to nodes and vice a versa
  
  #Edge Removal type and first iteration of Node removal type
  EdgeRemovalType <- as_data_frame(baseg) %>%
    select(from, to, Link) %>%
    left_join(TempEdges, by = c("Link" = "Names")) %>%
    rename(RemovalTypeEdge = RemovalType,
           RoundRemovedEdge = RoundRemoved) %>%
    left_join(TempNodes, by = c("from" = "Names")) %>%
    left_join(TempNodes, by = c("to" = "Names")) %>%
    mutate(
      #Conditioanlly class the edge removal type
      RemovalTypeEdge = case_when(
        RemovalTypeEdge == "Unknown" & (RemovalType.x == "Targeted"| RemovalType.y == "Targeted") ~ "Targeted",
        RemovalTypeEdge == "Unknown" ~ "Islanded",
        TRUE ~ RemovalTypeEdge
      ),
      #Conditionally class the node removaltype
      RemovalType.x = case_when(
        RemovalType.x =="Unknown" & RoundRemovedEdge == RoundRemoved.x & RemovalTypeEdge =="Overloaded" ~ "Overloaded",
        RemovalType.x == "Unknown" ~ "Islanded",
        TRUE ~ RemovalType.x
      ),
      RemovalType.y = case_when(
        RemovalType.y =="Unknown" & RoundRemovedEdge == RoundRemoved.y & RemovalTypeEdge =="Overloaded" ~ "Overloaded",
        RemovalType.y == "Unknown" ~ "Islanded",
        TRUE ~ RemovalType.y
      ))
  
  #Seperate Nodes and perform second iteration of Node removal type
  #This has to be done as due to the network structure nodes will appear multiple times in the previous dataframe and may have differe
  #Removal conditions each time
  NodeRemovalType <- bind_rows(select(EdgeRemovalType, Node = from, RoundRemoved = RoundRemoved.x, RemovalType = RemovalType.x),
                               select(EdgeRemovalType, Node =to, RoundRemoved = RoundRemoved.y, RemovalType = RemovalType.y))
  
  #Filter the node removal type
  Targeted<- NodeRemovalType %>%
    filter(RemovalType =="Targeted")
  
  Overloaded <-NodeRemovalType %>%
    filter(RemovalType != "Overloaded", !(Node %in% Targeted$Node))
  
  Islanded <-NodeRemovalType %>%
    filter(RemovalType != "Islanded", !(Node %in% Targeted$Node), !(Node %in% Overloaded$Node))
  
  #re-combine nodes
  #There can be multiple instances of each node if there are multiple edges on a node that match the same condition
  #e.g. two edges attach to a node that was targeted, that node will appear twice in the targeted df but nowhere else.
  #The distinct function prevents doubles
  NodeRemovalType <- bind_rows(Targeted, Overloaded, Islanded) %>%
    arrange(Node) %>%
    distinct(Node, .keep_all = TRUE)
  
  Out <- list(Node = NodeRemovalType, 
              Edge = EdgeRemovalType %>% 
                select(Link, RoundRemoved = RoundRemovedEdge, RemovalType= RemovalTypeEdge))
  
  return(Out)
  
  
}
