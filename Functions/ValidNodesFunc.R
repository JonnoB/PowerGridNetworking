ValidNodesFunc <-function(trans1, Impedance){
  #This function takes in the trans1 data frame and the impedance dataframe and outputs a dataframe of nodes to be included
  #in the power flow calculations
  
  ValidNodesCasc <- trans1 %>% 
    filter(Bus.Name %in% c(Impedance$Bus.1, Impedance$Bus.2)) 
  
  
}