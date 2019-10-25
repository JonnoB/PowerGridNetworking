#' Biggest Collapse
#' 
#' This function finds largest drop in performance for a given metric. Specifically it looks for the lowest
#' number in the given variable across all simulations and arranges them in rank order.
#' @param df A data frame. A dataframe describing the change in grid structure across attacks. Either the output of extract_attack_stats
#' or extract_network_stats
#' @param var bare unquoted variable node_name. Where you are interested in finding the biggest drop
#' @export
#' @seealso \code{\link{extract_network_stats}}, \code{\link{extract_attack_stats}}
#' @examples 
#' biggest_collapse(df, alpha)

biggest_collapse <- function(df, var){
  #This function finds largest drop in performance for a given metric. Specifically it looks for the lowest
  #number in the given variable across all simulations and arranges them in rank order.
  var <- enquo(var)
  df %>%
    group_by(Simulation) %>%
    arrange(!!(var)) %>%
    mutate(rank = 1:n()) %>%
    filter(rank == 1) %>%
    select(-rank) %>%
    ungroup
  #This code is all here as I can't get top n to work with quosure
}
