#' Capacity and electrical distance
#' 
#' This is a wrapper that makes the code for Net ability easier to read.
#' It only calculates the transfer capacity of the network and the electrical distance
#' @param g An igraph object
#' @param slack_ref the reference bus
#' @export

capacity_and_elecdist <- function(g, slack_ref){
  #This is a wrapper that makes the code for Net ability easier to read
  lemma <- imp_ptdf(g, slack_ref)
  message("Calculating the electrical distance")
  ElecDist <- electrical_distance(lemma$imp)
  message("Calculating the Capacity Matrix")
  CapacityMatrix <- transfer_capacity(g, lemma$ptdf)
  Out <-list(CapacityMatrix, ElecDist)
  names(Out)<- c("CapacityMatrix", "ElecDist")
  return(Out)
}
