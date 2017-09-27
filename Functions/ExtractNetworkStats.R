ExtractNetworkStats <- function( NetworkList){
  # Takes a list of lists of igraph objects where each list is an attack run
  # and the overall list shows the break down of the grid over a series of attacks
  #NetworkList: the out put of the AttackTheGrid function
  
  NetworkList %>% map_df(~{
    #find the last element of the list which is the final network for that Cascade
    g <- .x[[length(.x)]] 
    
    #extract maximum component size
    data.frame(MaxComp = components(g)$csize %>% max,
               TotalNodes = vcount(g),
               TotalEdges = ecount(g),
               PowerGen = sum(abs(get.vertex.attribute(g, "BalencedPower"))/2))
  }
   ) %>%
    mutate(NodesRemaining = first(TotalNodes) -0:(n()-1),
           GCfract = MaxComp/first(TotalNodes)
           )
  
}