#' Electrical Edge Centrality
#'
#' Calculates the electrical edge centrality of the graph
#'
#' This is an implementation of Wang et al 2011 Physica A, "An electrical betweeness approach for
#' vulnerability assesment of power grids considering the capacity generators and load
#' Can only be used on a single component graph, if the graph has multiple components calculate for each component
#'    individually and add the results together.
#'
#' @param g An igraph object of a power network.
#' @param Node_name Is the vertex attribute that contains the node names.
#' @param Generation The vertex attribute containing the node generation data.
#' @param Demand The vertex attribute containing the node generation data.
#' @param EdgeName A character string. The edge attribute that contains the edge names
# @param Bus_order Only used if no PDF is supplied. The vertex attribute that contains the rank order for slack reference.
#' @export
#'

ElectricalCentrality <- function(g, Node_name = "name",
                                 Generation = "Generation",
                                 Demand = "Demand",
                                 EdgeName = "Link"
                                # Bus_order = "Bus.Order"
                                 ){

  #Slack ref needs to be a node that neither produces nore consumes, aka it is a transfer node.
  #Filter to keep only transfernodes, produces many candidates due to components that were created when all non-transfer nodes were removed
  SlackRef <- igraph::delete_vertices(g,
                             igraph::get.vertex.attribute(g, Node_name)[igraph::get.vertex.attribute(g, Generation)!=0 |
                                                               igraph::get.vertex.attribute(g, Demand)!=0]) %>%
    SlackRefFunc(., Node_name, Generation = Generation)

  AZero <- CreateTransmission(g, EdgeName = EdgeName, VertexName = Node_name)
  LineProperties <- LinePropertiesMatrix(g, Edgename = EdgeName, Weight = "Y")

#This is removed as bus order is not really important, especially for nodes without generation or demand
    # #Take the transfer node with the lowest Bus.Order
  # SlackRef <-dplyr::data_frame(name = igraph::get.vertex.attribute(g, Node_name), Bus.order = igraph::get.vertex.attribute(g, Bus_order)) %>%
  #      dplyr::filter( name %in% SlackRef$name ) %>%
  #     dplyr::top_n(1, -Bus.order)
  SlackRef <- SlackRef %>% dplyr::slice(1)

  PTDF <- ImpPTDF(g,  SlackRef$name, AZero, LineProperties, EdgeName = EdgeName, VertexName = Node_name)$PTDF

  print("Creating Node Pair injection matrix")
  Combos <-NodePairs(g, Node_name = "name", Generation = Generation, Demand = Demand, PTDF = PTDF) %>%
    dplyr::filter(GenPair == TRUE) %>%
    dplyr::group_by(V1, V2) %>%
    dplyr::mutate(MinGen = min(abs(NetGen1), abs(NetGen2))) %>%
    dplyr::ungroup  %>%
    dplyr::mutate(Column = 1:dplyr::n()) #each pair needs its own column

  #Creates the Injection matrix and fills with zeros. This matrix is populated such that each column holds a node gen pair
  InjectionMat <- matrix(data = 0.0, nrow= ncol(PTDF), ncol = nrow(Combos))

  #Creates the (negative) demand column
  Combos2 <- Combos %>%
    dplyr::mutate(Demand = -MinGen)

  #Seperates out Generation and Demand then stacks them on each other
  Combos2 <-  dplyr::bind_rows(dplyr::select(Combos2, row = V1, MinGen, Column),
              dplyr::select(Combos2, row = V2, MinGen = Demand, Column))

  #Add in the power values for each node pair. Each node pair is a column and the row represents
  #the address of the node in the network.
  InjectionMat[Combos2$row + nrow(InjectionMat)*Combos2$Column - nrow(InjectionMat)] <- Combos2$MinGen

  #Find the absolute value of the line flows across all node pairs and sum
  print("Calculating edge scores")
  EdgeEC <- abs(PTDF %*% InjectionMat) %>% rowSums %>%
    dplyr::tibble(ElectricalCentrality = .) %>%
    purrr::set_names("ElectricalCentrality") %>%
    dplyr::mutate(Edgename = igraph::get.edge.attribute(g, EdgeName))

  print("Calculating node Electrical centrality scores")
  #Simplify the graph. This prevents lines being removed reducing the score of a node.
  EdgeEC2 <- EdgeEC %>%
   # dplyr::mutate(Edgename = sub("-[^-]+$", "", Edgename)) %>% #This was useful when there were parallel lines connecting node pairs.
    #These lines have now been added together, so this line has to be removed to prevent errors
    tidyr::separate(., Edgename, into = c("Bus.1", "Bus.2"), sep= "(-)") %>%
    dplyr::group_by(Bus.1, Bus.2) %>%
    dplyr::summarise(ElectricalCentrality = sum(ElectricalCentrality)) %>% #sum the scores for multple edges connecting two nodes
    dplyr::ungroup

  #create a graph from the new simplified edge list.
  #The graph edges have attributes of Electrical centrality scores.
  ECg <- EdgeEC2 %>%
    dplyr::select(Bus.1, Bus.2, ElectricalCentrality) %>%
    igraph::graph_from_data_frame(., directed = F)

  #create and adjacency matrix of the graph
  ECadj <- ECg %>%
    igraph::as_adjacency_matrix(., attr = "ElectricalCentrality", sparse = F)

  #sum the rows of the adjacency matrix to get the sum of the EC edge scores per node
  SumEdgeEC <- rowSums(ECadj) %>% #get sum of power
    as.matrix %>% #convert to matrix from named vector to get rownames
    dplyr::as_tibble( ., rownames = "Bus.Name") %>%#convert to df keeping rownames
    dplyr::rename(SumEdgeEC=V1)


  #create a data frame with the sum of injection vectors and the node names
  SumInjection <- dplyr::tibble(Bus.Name = colnames(PTDF), SumInjection = abs(rowSums(InjectionMat)))

  # add the sum of the edge score to the absolute value of the sum of the injection values at each node
  NodeEC <- dplyr::left_join(SumEdgeEC, SumInjection, by = "Bus.Name") %>%
    dplyr::mutate(NodeEC = (SumEdgeEC+SumInjection)/2) %>%
    dplyr::select(Bus.Name, NodeEC)

  Out <- list(EdgeEC, NodeEC)
  names(Out) <- c("EdgeEC", "NodeEC")
  return(Out)
}
