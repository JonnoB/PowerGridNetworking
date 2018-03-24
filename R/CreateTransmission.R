#' Create Transmission Network
#'
#' This function creates the transmission matrix between the edges and the In and Out Nodes
#'
#' The function follows the method described in Pepyne 2007.
#'    It takes a graph an ouputs a matrix which has -1,0-1 values which describe the nominal direction of flow.
#'    The function is typically used in conjunction with the LinePropertiesMatrix function. The function
#'    is a key part of calculating ImpPTDF.
#'
#' @param g an igraph object with attribute of named edges. This is an igraph object
#' @param Edgename The attribute name of the edges. This is a string
#' @export
#' @seealso \code{\link{LinePropertiesMatrix}} \code{\link{ImpPTDF}}
#' @example
#'  CreateTransmission(g, "Edgename")

CreateTransmission <- function(g, Edgename){
  #his function creates the transmission matrix between the edges and the In and Out Nodes
  #It follows the method set out in Pepyne 2007
  #It takes in a graph and outputs a numeric matrix which has -1,0-1 as values.

  #g: a network with attribute of named edges. This is an igraph object
  #Edgename: the attribute name of the edges, this is a character vector

  #Create the edge list
  Edgelist <- get.edgelist(g) %>%
    as_tibble %>%
    mutate(Link = get.edge.attribute(g, Edgename))

  #Produce the In and out dataframes

  In <- Edgelist %>%
    mutate(
      name = V1,
      Adjacent = 1) %>%
    select(-V1,-V2)

  Out <- Edgelist %>%
    mutate(
      name = V2,
      Adjacent = -1) %>%
    select(-V1,-V2)

  Transdf <- bind_rows(In, Out) %>%
    spread(., key = name, value = Adjacent, fill = 0)

  #Convert to a matrix
  Transmat <- Transdf %>%
    select(-Link) %>%
    .[match(get.edge.attribute(g, "Link"), Transdf$Link),
      match(get.vertex.attribute(g, "name"), names(.)) ] %>%
    #this step is necessary as tibbles cannot have row names
    as.matrix(., drop = FALSE)


  #name the rows.
  rownames(Transmat) <- Transdf$Link[match(get.edge.attribute(g, "Link"), Transdf$Link)]

  return(Transmat)
}
