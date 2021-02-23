BiggestCollapse <- function(df, var){
  #This function finds largest drop in performance for a given metric. Specifically it looks for the lowest
  #number in the given variable across all simulations and arranges them in rank order.


  var <- dplyr::enquo(var)


  df %>%
    dplyr::group_by(Simulation) %>%
    dplyr::arrange(!!(var)) %>%
    dplyr::mutate(rank = 1:dplyr::n()) %>%
    dplyr::filter(rank == 1) %>%
    dplyr::select(-rank) %>%
    dyplr::ungroup

  #This code is all here as I can't get top n to work with quosure

}
