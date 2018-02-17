#' Get load level of power-grid
#' Calculates the fraction of maximum flow in each edge across a power-grid
#'
#' Load level is used as a way of describing how close a power-grid or power line is to tripping.
#' Load level is the inverse of tolerance (\eqn{\alpha}).
#' @return A dataframe
#' @param g An igraph object representing a powergrid.
#' @param Edgename A character string. The default is "name".
#' @param Power A character String. The default is "PowerFlow".
#' @param Link.Limit A character string. The default is "Link.Limit".
#' @export
#' @example
#' LoadLevel(g)

LoadLevel <- function(g, EdgeName = "name", Power = "PowerFlow", Link.Limit = "Link.Limit"){
  df<- data_frame(Line.name = get.edge.attribute(g, name =  EdgeName),
                  PowerFlow = get.edge.attribute(g, name = Power),
                  Line.Limit = get.edge.attribute(g, name = Link.Limit)) %>%
    mutate(LineLoading = abs(PowerFlow)/Line.Limit)


  return(df)

}
