ElectricalDistance <- function(Imp){
  #Calculates the electrical distance used in Bompard et al 2009.
  #It takes in a matrix and outputs a matrix
  #Imp: the impedance matrix of the network
  
  nums  <- ncol(Imp)
  Combos <-as.matrix( expand.grid(1:nums, 1:nums))
  
  Z<- Imp
  Z[] <- Imp[cbind(Combos[,1], Combos[,1])] + Imp[cbind(Combos[,2], Combos[,2])] -2*Imp[Combos]
  


  
  return(Z)
  
}

