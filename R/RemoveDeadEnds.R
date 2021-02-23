#' Remove Dead Ends
#'
#'Removes Transfernodes of Degree 1 from the network. This prevents the network being padded with nodes that don't do anything
#'
#' @param g An igraph object representing a power network
#' @param  iteration the number of times the function has recursively removed nodes. Default start is 1
#' @export


RemoveDeadEnds <- function(g, iteration = 1){
  #This is a recursive function that removes transfer nodes of degree 1 iteratviely until there are non left.
  #This is becuase there shouldn't be any nodes like that.

  print(iteration)
  iteration <- iteration +1

  DeadEnds <- igraph::as_data_frame(g, what = "vertices") %>%
    tibble::as_tibble %>%
    dplyr::mutate(Degree = igraph::degree(g),
           NodeType = dplyr::case_when(
             BalencedPower<0 ~"Demand",
             BalencedPower==0~"Transfer",
             TRUE ~"Generation"
           )) %>%
    dplyr::filter(NodeType == "Transfer", Degree == 1)

   g2 <- igraph::delete.vertices(g, DeadEnds$name) %>%
    BalencedGenDem(., "Demand", "Generation")

   #if g and g2 are the same then the grid is stable and the process terminates
  if(igraph::vcount(g2)==igraph::vcount(g) & igraph::ecount(g2)==igraph::ecount(g)){
    return(g)
  } else{
    #otherwise repeat the function
    RemoveDeadEnds(g2, iteration)
  }

}
