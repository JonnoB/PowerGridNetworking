#' Create proportional loading line limits
#'
#' This function sets the line limits to the tolerance value \eqn{\alpha} chosen by the user.
#'
#'
#' @param g An igraph object representing a power grid. must have a power flow attribute.
#' @param alpha The system tolerance. A number larger than 1.
#' @param power_flow The node_name of the graph attribute which contains the power flow over each line.
#' @param edge_limit The attribute node_name that will be used when setting the line limits.
#' 
#' @return The original network with an additional edge attribute that is a numeric line limit created by; Line Limit =  \eqn{\alpha*power_flow}
#' 
#' @details 
#'    Proportional loading should only be used when there are no other options available as it can
#'    be challenging to set the value of \eqn{\alpha} close to the system mean.
#'    \href{https://arxiv.org/abs/1907.12848}{Bourne et al. 2019} find that values of \eqn{\alpha} that
#'    deviate substantially from the actual value of \eqn{\alpha} given that load profile will not represent
#'    the behaviour of the power grid under attack or collapse.
#' @export
#' @examples
#' proportional_load(g, alpha = 1.05)
proportional_load <-function(g, alpha, power_flow = "power_flow", edge_limit = "edge_limit"){
  LineLimits <- get.edge.attribute(g, power_flow) %>%
    abs(.)*alpha
  g_2 <- set_edge_attr(g, edge_limit, value = LineLimits)
  return(g_2)
}
