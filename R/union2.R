#' Merge two graphs
#'
#' Merges two graphs maintaining whilst their attributes.
#'
#' This function is an upgrade to the \pkg{igraph} function "union".
#'    It creates the union of two networks, and merges attributes with the same name.
#'    In the original union the graph attributes were lost.
#'    In the case where there are overlapping nodes the attributes of g1 take precedence
#'
#' @return A new graph object
#' @param g1 Igraph object
#' @param g2 Igraph object
#' @seealso \code{\link[igraph]{union.igraph}}
#' @export
#' @examples
#' union2(g1,g2)
#' Reduce(union2, list(g1,g2,g3))
#'
union2 <-function(g1, g2){

  g1_df_edge <- igraph::as_data_frame(g1)
  g2_df_edge <- igraph::as_data_frame(g2)

  g1_df_vertex <- igraph::as_data_frame(g1, what = "vertices")
  g2_df_vertex <- igraph::as_data_frame(g2, what = "vertices")


  g <- igraph::graph_from_data_frame(d = dplyr::bind_rows(g1_df_edge, g2_df_edge),
                        directed = FALSE,
                        vertices = dplyr::bind_rows(g1_df_vertex, g2_df_vertex))

  return(g)

}
