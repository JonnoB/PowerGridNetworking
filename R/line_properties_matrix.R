#' Create the line Properties matrix
#'
#' Creates the line properties matrix
#'
#'  This function creates a line properties matrix that can be used with the output of the Transmission matrix function.
#'  The function takes an igraph object of a power-grid and returns diagonal matrix. Where the diagonal is the edgeweight of
#'  each edge.
#'
#' @return A matrix, with names rows and columns
#' @param g Igraph object which contains an attribute naming the edges and another that provides the edge weight
#' @param edge_name A character string of the edge attribute that holds edge names. Default is "edge_name"
#' @param weight A character string of the edge.attribute for edge weights. Default is "Y
#' @export
#' @examples
#' This example doesn't show in the documentatio, I don't know why
#' g_2 <-make_ego_graph(g, 2, "AXMI")[[1]]
#' plot(g_2)
#' line_properties_matrix(g_2)
line_properties_matrix <- function(g, edge_name = "edge_name", weight = "y"){
  Link <- get.edge.attribute(g, edge_name)
  C <- matrix(data = 0, nrow = length(Link), ncol = length(Link))
  diag(C) <- get.edge.attribute(g, weight)
  colnames(C)<- Link
  rownames(C)<- Link
  return(C)
}
