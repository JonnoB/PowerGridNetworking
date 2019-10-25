#' Extend Attack
#'
#'  Extends a previous attack run with additional attack rounds
#'
#' @param network_list The output of attack_the_grid, which is to be extended
#' @param attack_order The original attack order used to generate the attack run
#' @param min_max_comp The minimum size of the maximum component, default is set to zero
#' @param total_attack_rounds The total number of attack rounds to execute in this extension
#' @export
#'
extend_attack <-  function(network_list, attack_order, min_max_comp = 0, total_attack_rounds = 1000 ){
  g<- network_list[[1]][[1]]
  #set strategy
  FixedNodes <- quo(fixed_strategy_attack(g, attack_order))
  #continue attack
  Fullgraph <- network_list %>%
    attack_the_grid(., FixedNodes, reference_grid = g, min_max_comp = min_max_comp, total_attack_rounds = total_attack_rounds)
}
