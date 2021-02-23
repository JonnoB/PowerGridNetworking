#' Get load level of power-grid
#' Calculates the fraction of maximum flow in each edge across a power-grid
#'
#' Load level is used as a way of describing how close a power-grid or power line is to tripping.
#' Load level is the inverse of tolerance (\eqn{\alpha}).
#' @return A dataframe
#' @param g An igraph object representing a powergrid.
#' @param EdgeName A character string. The default is "name".
#' @param PowerFlow A character String. The default is "PowerFlow".
#' @param Link.Limit A character string. The default is "Link.Limit".
#' @export
#' @examples
#' LoadLevel(g)

LoadLevel <- function(g, EdgeName = "name", PowerFlow = "PowerFlow", Link.Limit = "Link.Limit"){
  df<- dplyr::data_frame(Line.name = igraph::get.edge.attribute(g, name =  EdgeName),
                  PowerFlow = igraph::get.edge.attribute(g, name = PowerFlow),
                  Line.Limit = igraph::get.edge.attribute(g, name = Link.Limit)) %>%
    dplyr::mutate(LineLoading = abs(PowerFlow)/Line.Limit)


  return(df)

}
