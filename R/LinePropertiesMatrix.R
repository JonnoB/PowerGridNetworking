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
#' @param Edgename A character string of the edge attribute that holds edge names. Default is "Link"
#' @param Weight A character string of the edge.attribute for edge weights. Default is "Y
#' @export
#' @examples
#' This example doesn't show in the documentatio, I don't know why
#' g2 <-make_ego_graph(g, 2, "AXMI")[[1]]
#' plot(g2)
#' LinePropertiesMatrix(g2)


LinePropertiesMatrix <- function(g, Edgename = "Link", Weight = "Y"){
  Link <- get.edge.attribute(g, Edgename)
  C <- matrix(data = 0, nrow = length(Link), ncol = length(Link))
  diag(C) <- get.edge.attribute(g, Weight)
  colnames(C)<- Link
  rownames(C)<- Link

  return(C)
}
