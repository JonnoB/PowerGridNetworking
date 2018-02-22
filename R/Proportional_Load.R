#' Create proportional loading line limits
#' 
#' This function sets the line limits to the tolerance value \eqn{\alpha} chosen by the user.
#' 
#' 
#' @param g An igraph object representing a power grid. must have a power flow attribute.
#' @param alpha The system tolerance.
#' @param PowerFlow The name of the graph attribute which contains the power flow over each line.
#' @param Link.Limit The attribute name that will be used when setting the line limits.
#' @export
#' @example 
#' Proportional_Load(g, alpha = 1.05)
Proportional_Load <-function(g, alpha, PowerFlow = "PowerFlow", Link.Limit = "Link.Limit"){
  
  LineLimits <- get.edge.attribute(g, PowerFlow) %>%
    abs(.)*alpha
  
  g2 <- set_edge_attr(g, Link.Limit, value = LineLimits)
  
  return(g2)
  
}