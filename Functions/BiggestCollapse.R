BiggestCollapse <- function(df, var){
  #This function finds largest drop in performance for a given metric. Specifically it looks for the lowest
  #number in the given variable across all simulations and arranges them in rank order.
  
  
  var <- enquo(var)
  
  df %>%
    group_by(Simulation) %>%
    filter( UQ(var) == min(!!(var))) %>%
    ungroup %>%
    arrange(!!(var))
  
}