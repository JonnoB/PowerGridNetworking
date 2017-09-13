AdaptiveStrategyAttack <- function(g, AttackStrategy, Target = "Nodes", Number = 1){
  
  #g: the netowrk which is being attacked
  #Nodes: the Number of Nodes to Remove
  #seed: The random number generator to use. This is optional
  
  #Set the environment of the Attack strategy to inside the function so that the correct g is used
  AttackStrategy <- set_env(AttackStrategy, get_env())

  DeleteVect<- AttackStrategy %>%
    eval_tidy 

 g2 <-DeleteCarefully(g, Target, DeleteVect, Number)
  
  
  return(g2)
}