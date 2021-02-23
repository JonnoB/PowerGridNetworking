#' SlackRef
#'
#' Finds the Slack node in each component of the power grid. A graph is put in and a data frame giving
#' the slack reference for each component is returned. The Slack bus is the largest generator in that component
#'
#'
#' @param g The igraph object representing the power grid
#' @param name The vertex attribute containing node names
#' @param Generation The vertex attribute that contains the generation data.
#' @export
##Balance is spelt wrong have to change everywhere
SlackRefFunc <- function(g, name = "name", Generation = "Generation"){
  #Finds the slack ref in each component
  dplyr::tibble(name = igraph::get.vertex.attribute(g, name),
         Generation = igraph::get.vertex.attribute(g, Generation),
         component = igraph::components(g)$membership) %>%
    dplyr::group_by(component) %>%
    dplyr::arrange(dplyr::desc(Generation)) %>%
    dplyr::summarise(name = dplyr::first(name),
              Nodes = dplyr::n())
}
