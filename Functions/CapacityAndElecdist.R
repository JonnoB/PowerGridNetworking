CapacityAndElecdist <- function(g, SlackRef){
  #This is a wrapper that makes the code for Net ability easier to read
  
  message("Creating Power matrices")
  
  AZero <- CreateTransmission(g, "Link")
  
  # #remove Slack bus, usually the largest generator
  #drop = FALSE stops the matrix being converted to a vector when there are only two nodes in the sub-graph
  A <- AZero[,colnames(AZero)!=SlackRef, drop = FALSE]
  
  #Create the diagonal matrix of edge to itself impedance
  C <- LinePropertiesMatrix(g)
  
  B <- t(A) %*% C %*% A
  
  InjectionVector <- get.vertex.attribute(g, "BalencedPower")[get.vertex.attribute(g, "name")!=SlackRef]
  
  message("Inverting the Susceptance matrix")
  Imp <- solve(B)
  
  message("Creating the PTDF")
  
  PTDF <- C %*% A %*% Imp
  
  message("Calculating the electrical distance")
  
  ElecDist <- ElectricalDistance(Imp)
  
  message("Calculating the Capacity Matrix")
  
  CapacityMatrix <- TransferCapacity(g, PTDF)
  
  Out <-list(CapacityMatrix, ElecDist)
  names(Out)<- c("CapacityMatrix", "ElecDist")
  
  return(Out)
}