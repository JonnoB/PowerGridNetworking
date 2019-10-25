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
#' @param node_name Is the vertex attribute that contains the node names.
#' @param generation The vertex attribute containing the node generation data.
#' @param  demand The vertec attribute containing the node generation data.
#' @param bus_order Only used if no PDF is supplied. The vertex attribute that contains the rank order for slack reference.
#' @export
#'
electrical_centrality <- function(g, node_name = "name", generation = "generation", demand = "demand",  bus_order = "bus_order"){
  #Slack ref needs to be a node that neither produces nore consumes, aka it is a transfer node.
  #Filter to keep only transfernodes, produces many candidates due to components that were created when all non-transfer nodes were removed
  slack_ref<- delete_vertices(g,
                             get.vertex.attribute(g, node_name)[get.vertex.attribute(g, generation)!=0 |
                                                               get.vertex.attribute(g, demand)!=0]) %>%
    slack_ref_func(., node_name, bus_order)
  #Take the transfer node with the lowest Bus.Order
  slack_ref <-data_frame(node_name = get.vertex.attribute(g, node_name), Bus.order = get.vertex.attribute(g, bus_order)) %>%
       filter( node_name %in% slack_ref$node_name ) %>%
      top_n(1, -Bus.order)
  ptdf <- imp_ptdf(g,  slack_ref$node_name)$ptdf
  print("Creating Node Pair injection matrix")
  Combos <-node_pairs(g, node_name = "name", generation = "generation", demand = "demand", ptdf = ptdf) %>%
    filter(GenPair == TRUE) %>%
    group_by(V1, V2) %>%
    mutate(MinGen = min(abs(NetGen1), abs(NetGen2))) %>%
    ungroup  %>%
    mutate(Column = 1:n()) #each pair needs its own column
  #Creates the Injection matrix and fills with zeros. This matrix is populated such that each column holds a node gen pair
  InjectionMat <- matrix(data = 0.0, nrow= ncol(ptdf), ncol = nrow(Combos))
  #Creates the (negative) demand column
  Combos2 <- Combos %>%
    mutate(demand = -MinGen)
  #Seperates out generation and demand then stacks them on each other
  Combos2 <-  bind_rows(select(Combos2, row = V1, MinGen, Column),
              select(Combos2, row = V2, MinGen = demand, Column))
  #Add in the power values for each node pair. Each node pair is a column and the row represents
  #the address of the node in the network.
  InjectionMat[Combos2$row + nrow(InjectionMat)*Combos2$Column - nrow(InjectionMat)] <- Combos2$MinGen
  #Find the absolute value of the line flows across all node pairs and sum
  print("Calculating edge scores")
  EdgeEC <- abs(ptdf %*% InjectionMat) %>% rowSums %>%
    tibble(electrical_centrality = .) %>%
    set_names("ElectricalCentrality") %>%
    mutate(edge_name = get.edge.attribute(g, "name"))
  print("Calculating node Electrical centrality scores")
  #Simplify the graph. This prevents lines being removed reducing the score of a node.
  EdgeEC2 <- EdgeEC %>%
   # mutate(edge_name = sub("-[^-]+$", "", edge_name)) %>% #This was useful when there were parallel lines connecting node pairs.
    #These lines have now been added together, so this line has to be removed to prevent errors
    separate(., edge_name, into = c("Bus.1", "Bus.2"), sep= "(-)") %>%
    group_by(Bus.1, Bus.2) %>%
    summarise(electrical_centrality = sum(electrical_centrality)) %>% #sum the scores for multple edges connecting two nodes
    ungroup
  #create a graph from the new simplified edge list.
  #The graph edges have attributes of Electrical centrality scores.
  ECg <- EdgeEC2 %>%
    select(Bus.1, Bus.2, electrical_centrality) %>%
    graph_from_data_frame(., directed = F)
  #create and adjacency matrix of the graph
  ECadj <- ECg %>%
    as_adjacency_matrix(., attr = "ElectricalCentrality", sparse = F)
  #sum the rows of the adjacency matrix to get the sum of the EC edge scores per node
  SumEdgeEC <- rowSums(ECadj) %>% #get sum of power
    as.matrix %>% #convert to matrix from named vector to get rownames
    as_tibble( ., rownames = "Bus.Name") %>%#convert to df keeping rownames
    rename(SumEdgeEC=V1)
  #create a data frame with the sum of injection vectors and the node names
  SumInjection <- tibble(Bus.node_name = colnames(ptdf), SumInjection = abs(rowSums(InjectionMat)))
  # add the sum of the edge score to the absolute value of the sum of the injection values at each node
  NodeEC <- left_join(SumEdgeEC, SumInjection, by = "Bus.Name") %>%
    mutate(NodeEC = (SumEdgeEC+SumInjection)/2) %>%
    select(Bus.node_name, NodeEC)
  Out <- list(EdgeEC, NodeEC)
  names(Out) <- c("EdgeEC", "NodeEC")
  return(Out)
}
