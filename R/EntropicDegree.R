#' Dntropic Degree
#'
#' Calculates the entropic degree of each node in the network using the edge feature of choice.
#' The default is the flow limit of the line.
#'
#' This value is implemented as defined by Bompard et al in  "Analysis  of  structural
#'vulnerabilities  in  power  transmission  grids” doi:  10.1016/j.ijcip.2009.02.002.
#'
#' @param g an igraph object
#' @param value The line feature to be used in calculating the entropic degree
#' @param Scale Should the values be scaled by multiplying by the log of the node degree
#' @export
#'

EntropicDegree <- function(g, value = "Link.Limit", Scale = FALSE){

  igraph::E(g)$weight <- igraph::get.edge.attribute(g, name = value) %>% abs(.) #Otherwise negative values like power flow cause problems when logged

  ED <- (1-igraph::diversity(g))*igraph::strength(g)

  if(!Scale){
    ED <- ED * log(igraph::degree(g))
  }

  return(ED)
}
