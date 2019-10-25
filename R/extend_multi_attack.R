#' Extend Multi-Attack
#'
#'Allows a series of attack runs to be extended.
#'
#' @param files the files in the current working directory to be extended. These files should all have the extension'.rds'
#' @param deletion_order The output of multi_attack_order this should be the same as the original dataframe used for the simulations that are being extended
#' @param min_max_comp The minimum size of the maximum component, default is set to zero
#' @param total_attack_rounds The total number of attack rounds to execute in this extension
#' @seealso  \code{\link{multi_attack_order}} \code{\link{extend_attack}}
#' @export
#'
extend_mullti_attack <- function(files, deletion_order, min_max_comp = 0, total_attack_rounds = 1000){
  files %>% walk(~{
    print(.x)
    #get deletion order for current file
    deletion_order <-   deletion_order %>%
      filter(SimulationID == gsub(".rds","",.x)) %>%
      select(-SimulationID) %>%
      t %>%
      as.vector
    #Load networklist
    network_list <- readRDS(.x)
    #run extend_attack
    ExtendedNetworkList <- extend_attack(network_list, deletion_order)
    #save extended networkList
    saveRDS(ExtendedNetworkList, .x)
  })
}
