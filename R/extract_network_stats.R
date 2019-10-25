#' Summarise attack Statistics
#'
#'Sumamrises the attack statistics by extracting the information from
#'   the list of lists of graphs produced by the attack_the_grid function.
#'
#' The function summarises the effect of the attack on the network providing statistics on,
#' Max component size, Total nodes, Total Edges, Power Generated, Grid loading, Giant component fraction,
#' Blackout size.
#'
#' @param network_list The output of the AttackTheGridFunction
#' @param generation The node_name of the variable that stores the net generation data. character string
#' @param edge_name A character string. The default is "name".
#' @param power_flow A character String. The default is "power_flow".
#' @param edge_limit A character string. The default is "edge_limit".
#' @return A dataframe
#' @export
extract_network_stats <- function(network_list,  generation = "net_generation", edge_name = "name", power_flow = "power_flow", edge_limit = "edge_limit"){
  # Takes a list of lists of igraph objects where each list is an attack run
  # and the overall list shows the break down of the grid over a series of attacks
  #network_list: the out put of the attack_the_grid function
  network_list %>% map_df(~{
    #find the last element of the list which is the final network for that cascade
    g <- .x[[length(.x)]]
    #extract maximum component size
    data.frame(max_comp = components(g)$csize %>% max,
               total_nodes = vcount(g),
               total_edges = ecount(g),
               power_gen = sum(abs(get.vertex.attribute(g, generation))/2),
               mean_loading = mean(load_level(g, edge_name = edge_name, power_flow = power_flow, edge_limit = edge_limit)$LineLoading),
               median_loading = median(load_level(g, edge_name = edge_name, power_flow = power_flow, edge_limit = edge_limit)$line_loading),
               mean_degree  = mean(degree(g)), #allows the calculation of the molloy-reed criterion
               mean_degree_sqrd = mean(degree(g)^2)) #allows the calculation of the molloy-reed criterion
  }
  ) %>%
    mutate(NodesAttacked = 0:(n()-1),
           GCfract = (max(TotalNodes) - MaxComp)/max(TotalNodes),
           Blackout = (max(PowerGen) - PowerGen)/max(PowerGen)
    )
}
