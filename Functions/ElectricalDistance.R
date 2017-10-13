ElectricalDistance <- function(Imp){
  #Calculates the electrical distance used in Bompard et al 2009.
  #It takes in a matrix and outputs a matrix
  #Imp: the impedance matrix of the network
  
  nums  <- ncol(Imp)
  Combos <- expand.grid(1:nums, 1:nums)
  Z <-  map2_dbl(.x = Combos$Var1, .y = Combos$Var2, ~Imp[.x,.x] + Imp[.y,.y] -2*Imp[.x,.y]) %>%
    matrix(., nrow = nrow(Imp))
  
  return(Z)
  
}