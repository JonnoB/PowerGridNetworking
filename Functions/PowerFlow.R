PowerFlow <- function(Impedance, ValidNodes, SlackRef){
  # Takes the impedeance data frame, and a data frame of validnodes and caluclates the powr flow.
  # Impedance: A three column data frame of the impedeace between each connected node pair
  # ValidNodes: A data frame containing the columns "BusTransferA", "BusTransferB", "Bus.Name"
  
  AZero <-  Impedance %>%
    CreateTransmission(., "Bus.1", "Bus.2")
  
  # #remove Slack bus, usually the largest generator
  #drop = FALSE stops the matrix being converted to a vector when there are only two nodes in the sub-graph
  A <- AZero[,colnames(AZero)!=SlackRef$Bus.Name, drop = FALSE]
  
  #Create the diagonal matrix of 
  C <- LinePropertiesMatrix(Impedance, "Bus.1", "Bus.2")
  
  #The rows and columns have to be re-ordered to match the A matrix
  C<-C[match(rownames(A),rownames(C)), match(rownames(A),rownames(C))]
  
  B <- t(A) %*% C %*% A
  
  InjectionVector <- ValidNodes$BusTransferB[match(colnames(A), ValidNodes$Bus.Name)]
  
  PowerAlt <- C %*% A %*% solve(B, InjectionVector)
  
  PowerAlt <- data.frame(Link = rownames(PowerAlt), MW = PowerAlt[,1], stringsAsFactors = FALSE )

  return(PowerAlt)
}