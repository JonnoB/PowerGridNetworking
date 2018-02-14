BiggestCollapse <- function(df, var){
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