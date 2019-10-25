#' slack_ref
#'
#' Finds the Slack node in each component of the power grid. A graph is put in and a data frame giving
#' the slack reference for each component is returned. The Slack bus is the largest generator in that component
#'
#'
#' @param g The igraph object representing the power grid
#' @param node_name The vertex attribute containing node names
#' @param generation The vertex attribute that contains the generation data.
#' @export
##Balance is spelt wrong have to change everywhere
slack_ref_func <- function(g, node_name = "name", generation = "generation"){
  #Finds the slack ref in each component
  tibble(node_name = get.vertex.attribute(g, node_name),
         generation = get.vertex.attribute(g, generation),
         component = components(g)$membership) %>%
    group_by(component) %>%
    arrange(desc(generation)) %>%
    summarise(node_name = first(node_name),
              nodes = n())
}
