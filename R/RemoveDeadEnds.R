#' Remove Dead Ends
#'
#'Removes Transfernodes of Degree 1 from the network. This prevents the network being padded with nodes that don't do anything
#'
#' @param g An igraph object representing a power network
#' @param  iteration the number of times the function has recursively removed nodes. Default start is 1
#' @export


RemoveDeadEnds <- function(g, iteration = 1, Type = "Transfer"){
  #This is a recursive function that removes transfer nodes of degree 1 iteratviely until there are non left.
  #This is becuase there shouldn't be any nodes like that.

  print(iteration)
  iteration <- iteration +1

  DeadEnds <- as_data_frame(g, what = "vertices") %>%
    as.tibble %>%
    mutate(Degree = degree(g),
           NodeType = case_when(
             BalencedPower<0 ~"Demand",
             BalencedPower==0~"Transfer",
             TRUE ~"Generation"
           )) %>%
    filter(NodeType == "Transfer", Degree == 1)

   g2 <- delete.vertices(g, DeadEnds$name) %>%
    BalencedGenDem(., "Demand", "Generation")

   #if g and g2 are the same then the grid is stable and the process terminates
  if(vcount(g2)==vcount(g) & ecount(g2)==ecount(g)){
    return(g)
  } else{
    #otherwise repeat the function
    RemoveDeadEnds(g2, iteration)
  }

}
