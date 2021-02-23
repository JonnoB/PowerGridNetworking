#' Extend Multi-Attack
#'
#'Allows a series of attack runs to be extended.
#'
#' @param files the files in the current working directory to be extended. These files should all have the extension'.rds'
#' @param DeleteOrders The output of MultiAttackOrder this should be the same as the original dataframe used for the simulations that are being extended
#' @param MinMaxComp The minimum size of the maximum component, default is set to zero
#' @param TotalAttackRounds The total number of attack rounds to execute in this extension
#' @seealso  \code{\link{MultiAttackOrder}} \code{\link{ExtendAttack}}
#' @export
#'
ExtendMulltiAttack <- function(files, DeleteOrders, MinMaxComp = 0, TotalAttackRounds = 1000){
  
  files %>% purrr::walk(~{
    
    print(.x)
    #get deletion order for current file
    DeletionOrder <-   DeleteOrders %>%
      dplyr::filter(SimulationID == gsub(".rds","",.x)) %>%
      dplyr::select(-SimulationID) %>%
      t %>%
      as.vector
    
    #Load networklist
    NetworkList <- readRDS(.x)
    #run ExtendAttack
    ExtendedNetworkList <- ExtendAttack(NetworkList, DeletionOrder)
    
    #save extended networkList
    saveRDS(ExtendedNetworkList, .x)
    
  })
  
}
