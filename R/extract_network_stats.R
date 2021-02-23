#' Extract Network stats
#' 
#' This function extracts the attack statistics from the output of attack_the_grid
#' 
#' The function is about 55 times faster than ExtractNetworkStats
#' 
#' @param data_list A list. the output of attack_the_grid
#' 
#' @export
extract_network_stats <- function(data_list){
  
  Out <- dplyr::tibble( 
    attack_round = 0:(ncol(data_list$node_power)-1),
    generation =colSums(data_list$node_power*(data_list$node_power>0 & is.finite(data_list$node_power)), na.rm = TRUE),
    blackout_size = 1-generation/max(generation),
    nodes = colSums(is.finite(data_list$node_power)),
    edges = colSums(data_list$edge_status==0, na.rm = T),
    mean_loading = colMeans(abs(data_list$edge_power), na.rm = TRUE),
    median_loading = 1:ncol(data_list$edge_power) %>%
      purrr::map_dbl(~{stats::median(abs(data_list$edge_power[,.x]), na.rm = T)}),
    mean_alpha = colMeans(1/abs(data_list$edge_power), na.rm = TRUE), #1/mean_loading,#
    median_alpha = 1:ncol(data_list$edge_power) %>%
      purrr::map_dbl(~{stats::median(1/abs(data_list$edge_power[,.x]), na.rm = TRUE)}),
    #The smallest number this should ever be is 1. This is becuase any smaller and the network never had a GC
    gc_present = attack_round<data_list$gc_loss_round#colMeans(data_list$node_edge_count^2, na.rm = TRUE)> 2*colMeans(data_list$node_edge_count, na.rm = TRUE)
  ) %>%
    dplyr::mutate(    
      mean_loading = ifelse(blackout_size==1, 0, mean_loading),
      median_loading = ifelse(blackout_size==1, 0, median_loading),
      mean_alpha = ifelse(blackout_size==1, 0, mean_alpha),
      median_alpha = ifelse(blackout_size==1, 0, median_alpha)
    )
  return(Out)
  
}
