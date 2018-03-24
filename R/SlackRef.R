#' SlackRef
#' 
#' Finds the Slack node in each component of the power grid. A graph is put in and a data frame giving 
#' the slack reference for each component is returned
#' 
#' 
#' @param g The igraph object representing the power grid
#' @param name The vertex attribute containing node names
#' @param Bus.Order The vertex attribute that contains the rank order for slack reference
#' @export

SlackRefFunc <- function(g, name = "name", Bus.Order = "Bus.Order"){
  #Finds the slack ref in each component
  tibble(name = get.vertex.attribute(g, name),
         Bus.Order = get.vertex.attribute(g, Bus.Order),
         component = components(g)$membership) %>%
    group_by(component) %>%
    arrange(Bus.Order) %>%
    summarise(name = first(name),
              Nodes = n())
}