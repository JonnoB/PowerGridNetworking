#' Round Removed and type of Removal
#'
#' This function finds the round removed of all nodes and Edges in a network as well as whether the removal type was Targeted/Overloaded/Islanded
#'
#' The function accepts the list of lists produced by the function attack the grid and outputs a
#'     list containing two dataframes for nodes and Edges respectively.
#'
#' @param attack_series The list of lists produced by attack the grid
#' @export
round_type_removed <- function(attack_series){
  #finds the round removed for each type
  TempNodes <- attack_series %>% round_removed(type = "Node")
  TempEdges <- attack_series %>% round_removed(type = "edge")
  #gets the original graph
  baseg <- attack_series[[1]][[1]]
  #Join edges to nodes and vice a versa
  #Edge Removal type and first iteration of Node removal type
  EdgeRemovalType <- as_data_frame(baseg) %>%
    select(from, to, Link) %>%
    left_join(TempEdges, by = c("edge_name" = "Names")) %>%
    rename(RemovalTypeEdge = RemovalType,
           RoundRemovedEdge = round_removed) %>%
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
        RemovalType.x =="Unknown" & RoundRemovedEdge == round_removed.x & RemovalTypeEdge =="Overloaded" ~ "Overloaded",
        RemovalType.x == "Unknown" ~ "Islanded",
        TRUE ~ RemovalType.x
      ),
      RemovalType.y = case_when(
        RemovalType.y =="Unknown" & RoundRemovedEdge == round_removed.y & RemovalTypeEdge =="Overloaded" ~ "Overloaded",
        RemovalType.y == "Unknown" ~ "Islanded",
        TRUE ~ RemovalType.y
      ))
  #Seperate nodes and perform second iteration of Node removal type
  #This has to be done as due to the network structure nodes will appear multiple times in the previous dataframe and may have differe
  #Removal conditions each time
  NodeRemovalType <- bind_rows(select(EdgeRemovalType, node_name = from, round_removed = round_removed.x, RemovalType = RemovalType.x),
                               select(EdgeRemovalType, node_name =to, round_removed = round_removed.y, RemovalType = RemovalType.y))
  #Filter the node removal type
  Targeted<- NodeRemovalType %>%
    filter(RemovalType =="Targeted")
  Overloaded <-NodeRemovalType %>%
    filter(RemovalType != "Overloaded", !(node_name %in% Targeted$node_name))
  Islanded <-NodeRemovalType %>%
    filter(RemovalType != "Islanded", !(node_name %in% Targeted$node_name), !(node_name %in% Overloaded$node_name))
  #re-combine nodes
  #There can be multiple instances of each node if there are multiple edges on a node that match the same condition
  #e.g. two edges attach to a node that was targeted, that node will appear twice in the targeted df but nowhere else.
  #The distinct function prevents doubles
  NodeRemovalType <- bind_rows(Targeted, Overloaded, Islanded) %>%
    arrange(node_name) %>%
    distinct(node_name, .keep_all = TRUE)
  Out <- bind_rows(NodeRemovalType %>% mutate(type = "Node"),
              Edge = EdgeRemovalType %>%
                select(node_name = Link, round_removed = RoundRemovedEdge, RemovalType= RemovalTypeEdge) %>% mutate(type = "edge"))
  return(Out)
}
