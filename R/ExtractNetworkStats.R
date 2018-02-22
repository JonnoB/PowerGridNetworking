#' Summarise attack Statistics
#'
#'Sumamrises the attack statistics by extracting the information from
#'   the list of lists of graphs produced by the AttackTheGrid function.
#'
#' The function summarises the effect of the attack on the network providing statistics on,
#' Max component size, Total Nodes, Total Edges, Power Generated, Grid loading, Giant component fraction,
#' Blackout size.
#'
#' @param NetworkList The output of the AttackTheGridFunction
#' @return A dataframe
#' @export

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
               PowerGen = sum(abs(get.vertex.attribute(g, "BalencedPower"))/2),
               GridLoading = mean(LoadLevel(g)$LineLoading))
  }
   ) %>%
    mutate(NodesAttacked = 0:(n()-1),
           GCfract = (max(TotalNodes) - MaxComp)/max(TotalNodes),
           Blackout = (max(PowerGen) - PowerGen)/max(PowerGen)
           )

}
