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
  tibble(name = get.vertex.attribute(g, name),
         Generation = get.vertex.attribute(g, Generation),
         component = components(g)$membership) %>%
    group_by(component) %>%
    arrange(desc(Generation)) %>%
    summarise(name = first(name),
              Nodes = n())
}
