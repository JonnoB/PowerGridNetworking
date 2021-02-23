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
#' @param EdgeName A character string. The default is "name".
#' @param PowerFlow A character String. The default is "PowerFlow".
#' @param Link.Limit A character string. The default is "Link.Limit".
#' @return A dataframe
#' @export

ExtractNetworkStats <- function(NetworkList,
                                Generation = "BalencedPower",
                                EdgeName = "name",
                                PowerFlow = "PowerFlow",
                                Link.Limit = "Link.Limit"){
  # Takes a list of lists of igraph objects where each list is an attack run
  # and the overall list shows the break down of the grid over a series of attacks
  #NetworkList: the out put of the AttackTheGrid function

  #removes any trailing NULLs
  NetworkList<-NetworkList[1:(length(NetworkList)-sum(NetworkList %>% purrr::map_lgl(~is.null(.x[[1]]))))]

  NetworkList %>% purrr::map_df(~{
    #find the last element of the list which is the final network for that Cascade
    g <- .x[[length(.x)]]

    #extract maximum component size
    data.frame(largest_component = igraph::components(g)$csize %>% max,
               nodes = igraph::vcount(g),
               edges = igraph::ecount(g),
               generation = sum(abs(igraph::get.vertex.attribute(g, Generation))/2),
               #There are different ways of looking at the average loading/alpha as these values are inverted so can be greatly affected
               #by the method. Using these four methods will hopefully give a clearer picure.
               mean_loading = mean(LoadLevel(g, EdgeName = EdgeName, PowerFlow = PowerFlow, Link.Limit = Link.Limit)$LineLoading),
               median_loading = stats::median(LoadLevel(g, EdgeName = EdgeName, PowerFlow = PowerFlow, Link.Limit = Link.Limit)$LineLoading),
               mean_alpha = mean(1/LoadLevel(g, EdgeName = EdgeName, PowerFlow = PowerFlow, Link.Limit = Link.Limit)$LineLoading),
               median_alpha = stats::median(1/LoadLevel(g, EdgeName = EdgeName, PowerFlow = PowerFlow, Link.Limit = Link.Limit)$LineLoading),
               #Mean degree is usefull to know, however, it is essential if you want to calculate the point/time of giant component
               #collapse. That is why I have also included the mean of the degree squared
               mean_degree  = mean(igraph::degree(g)), #allows the calculation of the molloy-reed criterion
               mean_degree_sqrd = mean(igraph::degree(g)^2)) #allows the calculation of the molloy-reed criterion
  }
   ) %>%
    dplyr::mutate(attack_round = 0:(dplyr::n()-1), #This tells the total number of nodes or edges that have been attacked
           gc_fract = (max(nodes) - largest_component)/max(nodes),
           blackout_size = (max(generation) - generation)/max(generation),
           #converting to zero prevents NA or Inf values occuring when the network has no edges.
           #Generally this point of collapse may not be that interesting as the giant component has already collapsed.
           #But it causes a mess if you are looking at the last gasp of the network so I have included it.
           mean_loading = ifelse(blackout_size==1, 0, mean_loading),
           median_loading = ifelse(blackout_size==1, 0, median_loading),
           mean_alpha = ifelse(blackout_size==1, 0, mean_alpha),
           median_alpha = ifelse(blackout_size==1, 0, median_alpha)
           )

}
