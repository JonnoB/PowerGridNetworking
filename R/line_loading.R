#' Line loading
#' Calculates the fraction of maximum flow in each edge across a power-grid
#'
#' Line loading is used as a way of describing how close a power-grid or power line is to tripping.
#' Line loading is the inverse of tolerance (\eqn{\alpha}).
#' @return A dataframe
#' @param g An igraph object representing a powergrid.
#' @param edge_name A character string. The default is "name".
#' @param power_flow A character String. The default is "power_flow".
#' @param link_limit A character string. The default is "link_limit".
#' @export
#' @examples
#' load_level(g)
line_loading <- function(g, edge_name = "name", power_flow = "power_flow", edge_limit = "edge_limit"){
  df<- data_frame(edge_name = get.edge.attribute(g, node_name =  edge_name),
                  power_flow = get.edge.attribute(g, node_name = power_flow),
                  edge_limit = get.edge.attribute(g, node_name = edge_limit)) %>%
    mutate(line_loading = abs(power_flow)/edge_limit)
  return(df)
}
