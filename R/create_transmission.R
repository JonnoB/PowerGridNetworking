#' Create Transmission Network
#'
#' This function creates the transmission matrix between the edges and the In and Out nodes
#'
#' The function follows the method described in Pepyne 2007.
#'    It takes a graph an ouputs a matrix which has -1,0-1 values which describe the nominal direction of flow.
#'    The function is typically used in conjunction with the line_properties_matrix function. The function
#'    is a key part of calculating imp_ptdf.
#'
#' @param g an igraph object with attribute of named edges. This is an igraph object
#' @param edge_name The attribute node_name of the edges. This is a string
#' @param node_name the variable that holds the names of the nodes. a character string.
#' @export
#' @seealso \code{\link{line_properties_matrix}} \code{\link{imp_ptdf}}
#' @examples
#'  create_transmission(g,  edge_name = "edge_name", node_name = "name")
create_transmission <- function(g, edge_name = "edge_name", node_name = "name"){
  #This function creates the transmission matrix between the edges and the In and Out nodes
  #It follows the method set out in Pepyne 2007
  #It takes in a graph and outputs a numeric matrix which has -1,0-1 as values.
  #g: a network with attribute of named edges. This is an igraph object
  #edge_name: the attribute node_name of the edges, this is a character vector
  #Create the edge list
  Edgelist <- get.edgelist(g) %>%
    as_tibble %>%
    mutate(Link = get.edge.attribute(g, edge_name))
  #Produce the In and out dataframes
  In <- Edgelist %>%
    mutate(
      node_name = V1,
      Adjacent = 1) %>%
    select(-V1,-V2)
  Out <- Edgelist %>%
    mutate(
      node_name = V2,
      Adjacent = -1) %>%
    select(-V1,-V2)
  Transdf <- bind_rows(In, Out) %>%
    spread(., key = node_name, value = Adjacent, fill = 0)
  #Convert to a matrix
  Transmat <- Transdf %>%
    select(-Link) %>%
    .[match(get.edge.attribute(g, edge_name), Transdf$Link),
      match(get.vertex.attribute(g, "name"), names(.)) ] %>%
    #this step is necessary as tibbles cannot have row names
    as.matrix(., drop = FALSE)
  #node_name the rows.
  rownames(Transmat) <- Transdf$Link[match(get.edge.attribute(g, edge_name), Transdf$Link)]
  return(Transmat)
}
