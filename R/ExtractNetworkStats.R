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
#' @param Generation The name of the variable that stores the net generation data. character string
#' @param Edgename A character string. The default is "name".
#' @param PowerFlow A character String. The default is "PowerFlow".
#' @param Link.Limit A character string. The default is "Link.Limit".
#' @return A dataframe
#' @export

ExtractNetworkStats <- function(NetworkList,  Generation = "BalencedPower", EdgeName = "name", PowerFlow = "PowerFlow", Link.Limit = "Link.Limit"){
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
               PowerGen = sum(abs(get.vertex.attribute(g, Generation))/2),
               GridLoading = mean(LoadLevel(g, EdgeName = EdgeName, PowerFlow = PowerFlow, Link.Limit = Link.Limit)$LineLoading),
               mean_degree  = mean(degree(g)), #allows the calculation of the molloy-reed criterion
               mean_degree_sqrd = mean(degree(g)^2)) #allows the calculation of the molloy-reed criterion
  }
   ) %>%
    mutate(NodesAttacked = 0:(n()-1),
           GCfract = (max(TotalNodes) - MaxComp)/max(TotalNodes),
           Blackout = (max(PowerGen) - PowerGen)/max(PowerGen)
           )

}
