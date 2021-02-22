#' NetworkStats
#' Creates summary statistics of the network
#'
#' The function takes a graph and returns a set of common summary graph statistics to understand the graph topology.
#' These statistics are, number of node, number of edges, number of components, Betweeness, Mean Degree, Assortivity, Clustering, and Mean Distance.
#' @param g An igraph object
#' @param colname A character string. The desired name of the output statistic column. the default is "value"
#' @export

NetworkStats <- function(g, colname = "value"){
  tibble(Metric = c("Nodes", "Edges", "Components" ,"Betweenness", "Degree", "Assortativity", "Clustering", "Distance"),
             value = c(vcount(g), ecount(g), components(g)$no, mean(betweenness(g)),mean(degree(g)), assortativity(g, degree(g)),
                       transitivity(g), mean_distance(g, directed = F))
  ) %>% setNames(c("Metric", colname))

  #This takes forever maybe should replace with *distances* or something else? and custome electrical distance function
  #  mean( ifelse(is.finite(shortest.paths(g)),shortest.paths(g), NA), na.rm = T)
}
