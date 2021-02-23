#' Find components of a graph that have changed
#' Find components of a graph that have changed
#'
#' Using this should be able to cut down on the amount of calculation done on the power flow equations
#' This function checks to see which subcomponents have changed in the graph and which are the same.
#' It does this by comparing edges. This is because a component may have had an edge removed but still be intact.
#' However, the powerflow will still need to be re-calculated.
#' The function a scaler vector representing each of the changed components
#' @param g the current graph
#' @param g0 the previous graph
#' @param EdgeName A character string. The name of the edge attribute
#' @export
#'
Components_differ <-function(g, g0, EdgeName = "name"){

  #what edge/s is/are missing in the new graph?
  g0_df <- igraph::as_data_frame(g0)
  
  #A list containing the edge and vertex dataframes for g.
  #Both these elements are needed
  g_df <- igraph::as_data_frame(g, what = "both")

  #testing code  
  #This shows that the component membership and the vertex data frame are in the same order
  # test <- g_df$vertices %>%
  #   dplyr::mutate(component = igraph::components(g)$membership) %>%
  #   dplyr::filter(name %in% effected_nodes)

  #the edges that are in g0 but not in the subsequent graph g
  lost_edges <- g0_df[, EdgeName][!(g0_df[, EdgeName] %in% g_df$edges[, EdgeName])]
  
  #The unique list of from and to nodes that were touching the missing edges in g0
  effected_nodes <-unique(c(g0_df[g0_df[,EdgeName] %in% lost_edges, 1], #from nodes
                            g0_df[g0_df[,EdgeName] %in% lost_edges, 2])) #to nodes
  
  Out <- unique(igraph::components(g)$membership[g_df$vertices$name %in% effected_nodes])

  return(Out)
  
  }
