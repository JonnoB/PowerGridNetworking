CapacityAndElecdist <- function(g, SlackRef){
  #This is a wrapper that makes the code for Net ability easier to read
  
  lemma <- ImpPTDF(g, SlackRef)
  
  message("Calculating the electrical distance")
  
  ElecDist <- ElectricalDistance(lemma$Imp)
  
  message("Calculating the Capacity Matrix")
  
  CapacityMatrix <- TransferCapacity(g, lemma$PTDF)
  
  Out <-list(CapacityMatrix, ElecDist)
  names(Out)<- c("CapacityMatrix", "ElecDist")
  
  return(Out)
}