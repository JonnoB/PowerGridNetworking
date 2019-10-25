#' Remove Dead Ends
#'
#'Removes Transfernodes of Degree 1 from the network. This prevents the network being padded with nodes that don't do anything
#'
#' @param g An igraph object representing a power network
#' @param  iteration the number of times the function has recursively removed nodes. Default start is 1
#' @export
remove_dead_ends <- function(g, iteration = 1, type = "transfer"){
  #This is a recursive function that removes transfer nodes of degree 1 iteratviely until there are non left.
  #This is becuase there shouldn't be any nodes like that.
  print(iteration)
  iteration <- iteration +1
  DeadEnds <- as_data_frame(g, what = "vertices") %>%
    as.tibble %>%
    mutate(Degree = degree(g),
           NodeType = case_when(
             BalencedPower<0 ~"demand",
             BalencedPower==0~"transfer",
             TRUE ~"generation"
           )) %>%
    filter(NodeType == "transfer", Degree == 1)
   g_2 <- delete.vertices(g, DeadEnds$node_name) %>%
    balance_generation_and_demand(., "demand", "generation")
   #if g and g_2 are the same then the grid is stable and the process terminates
  if(vcount(g_2)==vcount(g) & ecount(g_2)==ecount(g)){
    return(g)
  } else{
    #otherwise repeat the function
    remove_dead_ends(g_2, iteration)
  }
}
