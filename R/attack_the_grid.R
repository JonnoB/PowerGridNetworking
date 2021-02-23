#' Initiate power-grid attack simulation
#'
#' This function simulates an attack on the power grid using the parameter settings you choose
#'    the outut of the function is a nested list of igraph objects.
#' @param g An igraph object. The graph that will be attacked
#' the network list is simply list(list(g)).
#' @param AttackStrategy A function that calculates which node to delete the function is is in "quo" form and embedded in an
#' attack type.
#' @param g0 The grid that will be used to test the largest component against if NULL it uses the given network.
#' @param TotalAttackRounds The maximum number of nodes to be removed before the process stops.
#' @param CascadeMode Whether the power flow equations will be used to check line-overloading or not.
#' @param Demand the name of the node Load variable. A character string.
#' @param Generation the name of the node generation variable. A character string.
#' @param EdgeName the variable that holds the edge names, a character string.
#' @param VertexName the variable that holds the names of the nodes, to identify the slack ref. a character string
#' @param Net_generation the name that the net generation data for each node is held in
#' @param power_flow A character string. This value indicates the name of the edge attribute that holds power flow, the default is "PowerFlow"
#' @param edge_capacity A character string. This value indicates the name of the edge attribute that holds the edge limit, the default is "Link.Limit"
#' @param target A character string. This value defines the columns of the returned it must match the target element of the attack strategy or an
#' error will be thrown if the number of columns of the matrix is exceeded. modularising the function call to attack strategy
#' would solve this, but it is a low priority.
#' @export
#' @examples
#' AttackTheGrid(NetworkList, AttackStrategy, SubstationData, EdgeData, g0 = NULL)
#' Out <- AttackTheGrid(NetworkList,
#' AttackStrategy,
#' g0 = NULL,
#' TotalAttackRounds=100,
#' CascadeMode = TRUE,
#' CumulativeAttacks = NULL)

#Version of attack the grid that outputs a list of matrices. This should be much smaller files and
# quicker to summarise
attack_the_grid <- function(g,
                           AttackStrategy,
                           g0 = NULL,
                           TotalAttackRounds = 1000,
                           CascadeMode = TRUE,
                           Demand = "demand",
                           Generation = "generation",
                           EdgeName = "edge_name",
                           VertexName = "name",
                           Net_generation = "net_generation",
                           power_flow = "power_flow",
                           edge_capacity = "edge_capacity",
                           target = "nodes"
){

  #I can change the function so that only a graph need be entered and a list of graphs is returned. This is
  #becuase I am no longer recursing the function.

  #Entering a graph instead of a list would make this tosh simpler
  #g <- NetworkList[[length(NetworkList)]]

  total_nodes <- igraph::vcount(g)
  total_edges <- igraph::ecount(g)

  if(target =="nodes"){

    total_columns <- total_nodes + 1

  } else if(target == "edges"){
    total_columns <- total_edges + 1
  } else{
    stop("target must be either 'edges' or 'nodes'")

  }

  edge_capacity_vector <- igraph::edge_attr(g, name = edge_capacity)

  node_names <- igraph::vertex_attr(g, name = VertexName)
  edge_names <- igraph::edge_attr(g, name = EdgeName)

  node_power <- matrix(NA, nrow = total_nodes, ncol = total_columns, dimnames = list(node_names))
  node_edge_count <- matrix(NA, nrow = total_nodes, ncol = total_columns, dimnames = list(node_names))
  edge_power <-  matrix(NA, nrow = total_edges, ncol = total_columns, dimnames = list(edge_names))
  edge_status <-  matrix(NA, nrow = total_edges, ncol = total_columns, dimnames = list(edge_names))

  node_vector <- rep(NA, length = total_nodes)
  edge_vector <- rep(NA, length = total_edges)

  #This if statement only needs to be done once not every time. A small change but makes it easier to read.
  if(is.null(g0)){
    g0  <- g
  }

  #precalculation of the line and transmission matrices
  #This speeds up the PTDF function by reducing expensive operations (Transmission more than LineProperties)
  AZero <- CreateTransmission(g, EdgeName, VertexName)
  LineProperties <- LinePropertiesMatrix(g, EdgeName, Weight = "Y")

  GridCollapsed <- FALSE
  TopoStability <- FALSE
  CumulativeAttacks <- 0

  ###
  #
  #Add in initial data on the first column of the matrices
  #
  ####
  edge_order <- match(edge_names, igraph::edge_attr(g, name = EdgeName))
  node_order <- match(node_names, igraph::vertex_attr(g, name = VertexName))

  #
  edge_status[edge_order, CumulativeAttacks + 1] <-0
  edge_power[edge_order, CumulativeAttacks + 1] <- igraph::edge_attr(g, name = power_flow) / edge_capacity_vector
  node_power[node_order, CumulativeAttacks + 1] <- igraph::vertex_attr(g, name = Net_generation)
  node_edge_count[node_order, CumulativeAttacks + 1] <- igraph::degree(g)

  #The stop conditdions are a little over the top
  while (!(CumulativeAttacks==TotalAttackRounds| GridCollapsed| TopoStability)) {

    CumulativeAttacks <- CumulativeAttacks + 1
    #print(CumulativeAttacks)
    #Remove the desired part of the network.
    g2 <- AttackStrategy %>%
      rlang::eval_tidy(., data = list(g = g)) #The capture environment contains delete nodes, however the current g is fed in here

    ####
    #
    #This bit of code shows whether edges and nodes have been lost through direct targetting
    #
    ####
    #edges in original network but and g
    edge_orig_and_g <- edge_names %in% igraph::edge_attr(g, name = EdgeName)
    #Edges in original network but and g2
    edge_orig_and_g2 <- edge_names %in% igraph::edge_attr(g2, name = EdgeName)

    #edges in original network but and g
    nodes_orig_and_g <- node_names %in% igraph::vertex_attr(g, name = VertexName)
    #Edges in original network but and g2
    nodes_orig_and_g2 <- node_names %in% igraph::vertex_attr(g2, name = VertexName)

    #The difference between g2 and g3
    edge_diff <- edge_orig_and_g != edge_orig_and_g2
    node_diff <- nodes_orig_and_g != nodes_orig_and_g2
    #Replace the NA value in the matrix with the loss through targeting code
    #In the case all is FALSE an error is thrown. This if statement avoids the update in that case
    if(!all(edge_diff==FALSE)){
      edge_status[edge_diff, CumulativeAttacks + 1] <- 1L
    }
    if(!all(node_diff==FALSE)){
      node_power[node_diff, CumulativeAttacks + 1] <- -Inf
    }

    ##Rebalence network
    # #This means that the Cascade calc takes a balanced network which is good, generation or demand nodes may have been removed
    #this needs to be accounted for
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
    ###
    #
    #Replace the NA value in the matrix with the loss through islanding code
    #
    ##
    #The difference between g2 and g3
    edge_diff <- edge_orig_and_g != edge_orig_and_g2
    node_diff <- nodes_orig_and_g != nodes_orig_and_g2
    #Replace the NA value in the matrix with the loss through islanding code
    #In the case all is FALSE an error is thrown. This if statement avoids the update in that case
    if(!all(edge_diff==FALSE)){
    edge_status[edge_diff, CumulativeAttacks + 1] <- 2L
    }
    if(!all(node_diff==FALSE)){
    node_power[node_diff, CumulativeAttacks + 1] <- +Inf
    }

    ##
    #
    ##

    GridCollapsed <- igraph::ecount(g3)==0

    #This If statement prevents Cascading if theire are no cascadable components
    if(!GridCollapsed){

      if(CascadeMode){
        #this returns a list of networks each of the cascade
        CascadeOut <- Cascade2(g  = g3,
                               g0 = g,
                               node_power = node_power[,CumulativeAttacks + 1],
                               edge_status = edge_status[,CumulativeAttacks + 1],
                               AZero = AZero,
                               LineProperties = LineProperties,
                               Demand = Demand,
                               Generation = Generation,
                               EdgeName = EdgeName,
                               VertexName = VertexName,
                               Net_generation = Net_generation,
                               power_flow = power_flow,
                               edge_limit = edge_capacity
        )
        #edge and node status updated here before returning outside the if statement
        edge_status[, CumulativeAttacks + 1] <- CascadeOut$edge_status
        node_power[, CumulativeAttacks + 1] <- CascadeOut$node_power

        #update g with the result of cascade
        g3 <- CascadeOut$g

      }

      # message(paste("Attack ",CumulativeAttacks, " Nodes Remaining", igraph::vcount(gCasc[[length(gCasc)]])))

    }

    #Checks to see if the topology of the network is unchanged.
    #If this is TRUE then nothing is being removed and the process can stop
    TopoStability <- (igraph::vcount(g3) == igraph::vcount(g) &   igraph::ecount(g3) == igraph::ecount(g))

    g <- g3

    edge_order <- match(igraph::edge_attr(g, name = EdgeName), edge_names, nomatch = 0)
    node_order <- match(igraph::vertex_attr(g, name = VertexName), node_names, nomatch = 0)

    #Replace the NA value in the matrix with the loss through islanding code
    edge_status[edge_order, CumulativeAttacks + 1] <- 0L
    edge_power[edge_order, CumulativeAttacks + 1] <- igraph::edge_attr(g, name = power_flow)
    #This is inserted here as it is an easy way to convert to load level without having to use matrix algebra outside the loop
    edge_power[, CumulativeAttacks + 1] <- edge_power[, CumulativeAttacks + 1] / edge_capacity_vector
    node_power[node_order, CumulativeAttacks + 1] <- igraph::vertex_attr(g, name = Net_generation)
    node_edge_count[node_order, CumulativeAttacks + 1] <- igraph::degree(g)
  }

  #remove the columns which are just NA's
  edge_status <- edge_status[, 1:(CumulativeAttacks + 1)]
  edge_power  <- abs(edge_power[,  1:(CumulativeAttacks + 1)])# %*% diag(1/igraph::edge_attr(g0, name = edge_capacity))   #edge capacity
  node_power  <- node_power[,  1:(CumulativeAttacks + 1)]
  node_edge_count <- node_edge_count[, 1:(CumulativeAttacks + 1)]

  #There is a question mark over whether the node_edge_count should be kept or simply return the round in which the giant component
  #is lost. for now only the round is returned, but the full matrix can be returned if necessary
  return(list(node_power = node_power,
              #node_edge_count = node_edge_count,
              edge_status = edge_status,
              edge_power = edge_power,
              #The -1 at the end is becuase the first column of the matrix i attack 0 and doesn't count
              #The number is the first attack round which once completed has no GC. All previous round do have a GC
              gc_loss_round = sum(colMeans(node_edge_count^2, na.rm = TRUE)> 2*colMeans(node_edge_count, na.rm = TRUE))-1
             ))
}
