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
#' @param EdgeName The attribute name of the edges. This is a string
#' @param VertexName the variable that holds the names of the nodes. a character string.
#' @export
#' @seealso \code{\link{LinePropertiesMatrix}} \code{\link{ImpPTDF}}
#' @examples
#'  CreateTransmission(g,  EdgeName = "Link", VertexName = "name")

CreateTransmission <- function(g, EdgeName = "Link", VertexName = "name"){
  #his function creates the transmission matrix between the edges and the In and Out Nodes
  #It follows the method set out in Pepyne 2007
  #It takes in a graph and outputs a numeric matrix which has -1,0-1 as values.

  #g: a network with attribute of named edges. This is an igraph object
  #EdgeName: the attribute name of the edges, this is a character vector

  #Create the edge list
  Edgelist <- igraph::get.edgelist(g) %>%
    tibble::as_tibble(., .names_repair = "universal") %>%
    rlang::set_names(c("V1", "V2")) %>%
    dplyr::mutate(Link = igraph::get.edge.attribute(g, EdgeName))

  #Produce the In and out dataframes

  In <- Edgelist %>%
    dplyr::mutate(
      name = V1,
      Adjacent = 1) %>%
    dplyr::select(-V1,-V2)

  Out <- Edgelist %>%
    dplyr::mutate(
      name = V2,
      Adjacent = -1) %>%
    dplyr::select(-V1,-V2)

  Transdf <- dplyr::bind_rows(In, Out) %>%
    tidyr::spread(., key = name, value = Adjacent, fill = 0)

  #Convert to a matrix
  Transmat <- Transdf %>%
    dplyr::select(-Link) %>%
    .[match(igraph::get.edge.attribute(g, EdgeName), Transdf$Link),
      match(igraph::get.vertex.attribute(g, "name"), names(.)) ] %>%
    #this step is necessary as tibbles cannot have row names
    as.matrix(., drop = FALSE)


  #name the rows.
  rownames(Transmat) <- Transdf$Link[match(igraph::get.edge.attribute(g, EdgeName), Transdf$Link)]
  #Convert into a sparse matrix
  Transmat <- Matrix::Matrix(Transmat)

  return(Transmat)
}
