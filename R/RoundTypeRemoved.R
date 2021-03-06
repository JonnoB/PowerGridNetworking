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
  EdgeRemovalType <- igraph::as_data_frame(baseg) %>%
    dplyr::select(from, to, Link) %>%
    dplyr::left_join(TempEdges, by = c("Link" = "Names")) %>%
    dplyr::rename(RemovalTypeEdge = RemovalType,
           RoundRemovedEdge = RoundRemoved) %>%
    dplyr::left_join(TempNodes, by = c("from" = "Names")) %>%
    dplyr::left_join(TempNodes, by = c("to" = "Names")) %>%
    dplyr::mutate(
      #Conditioanlly class the edge removal type
      RemovalTypeEdge = dplyr::case_when(
        RemovalTypeEdge == "Unknown" & (RemovalType.x == "Targeted"| RemovalType.y == "Targeted") ~ "Targeted",
        RemovalTypeEdge == "Unknown" ~ "Islanded",
        TRUE ~ RemovalTypeEdge
      ),
      #Conditionally class the node removaltype
      RemovalType.x = dplyr::case_when(
        RemovalType.x =="Unknown" & RoundRemovedEdge == RoundRemoved.x & RemovalTypeEdge =="Overloaded" ~ "Overloaded",
        RemovalType.x == "Unknown" ~ "Islanded",
        TRUE ~ RemovalType.x
      ),
      RemovalType.y = dplyr::case_when(
        RemovalType.y =="Unknown" & RoundRemovedEdge == RoundRemoved.y & RemovalTypeEdge =="Overloaded" ~ "Overloaded",
        RemovalType.y == "Unknown" ~ "Islanded",
        TRUE ~ RemovalType.y
      ))

  #Seperate Nodes and perform second iteration of Node removal type
  #This has to be done as due to the network structure nodes will appear multiple times in the previous dataframe and may have differe
  #Removal conditions each time
  NodeRemovalType <- dplyr::bind_rows(dplyr::select(EdgeRemovalType, Name = from, RoundRemoved = RoundRemoved.x, RemovalType = RemovalType.x),
                               dplyr::select(EdgeRemovalType, Name =to, RoundRemoved = RoundRemoved.y, RemovalType = RemovalType.y))

  #Filter the node removal type
  Targeted<- NodeRemovalType %>%
    dplyr::filter(RemovalType =="Targeted")

  Overloaded <-NodeRemovalType %>%
    dplyr::filter(RemovalType != "Overloaded", !(Name %in% Targeted$Name))

  Islanded <-NodeRemovalType %>%
    dplyr::filter(RemovalType != "Islanded", !(Name %in% Targeted$Name), !(Name %in% Overloaded$Name))

  #re-combine nodes
  #There can be multiple instances of each node if there are multiple edges on a node that match the same condition
  #e.g. two edges attach to a node that was targeted, that node will appear twice in the targeted df but nowhere else.
  #The distinct function prevents doubles
  NodeRemovalType <- dplyr::bind_rows(Targeted, Overloaded, Islanded) %>%
    dplyr::arrange(Name) %>%
    dplyr::distinct(Name, .keep_all = TRUE)

  Out <- dplyr::bind_rows(NodeRemovalType %>% dplyr::mutate(type = "Node"),
              Edge = EdgeRemovalType %>%
                dplyr::select(Name = Link, RoundRemoved = RoundRemovedEdge, RemovalType= RemovalTypeEdge) %>% dplyr::mutate(type = "Edge"))

  return(Out)


}
