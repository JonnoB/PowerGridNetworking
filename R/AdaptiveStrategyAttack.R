#' Make Strategy Adaptive
#'
#' This implements an attack strategy to be adaptive.
#'     This means that it will be re-evaluated every
#'     round to choose the most effective point of attack
#'     according to that metric
#' @param g The network which is being attacked
#' @param Nodes The Number of Nodes to Remove
#' @param seed The random number generator seed to use. This is optional
#' @param Number The Number of targets to generate
#' @keywords removal regime
#' @export
#' @examples
#' AdaptiveStrategyAttack <- function(g, AttackStrategy, Target = "Nodes", Number = 1)

#Set the environment of the Attack strategy to inside the function so that the correct g is used


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
