#' Make Strategy Adaptive
#'
#' This implements an attack strategy to be adaptive.
#'     This means that it will be re-evaluated every
#'     round to choose the most effective point of attack
#'     according to that metric
#' @param g The network which is being attacked
#' @param nodes The number of nodes to Remove
#' @param seed The random number generator seed to use. This is optional
#' @param number The number of targets to generate
#' @keywords removal regime
#' @export
#' @examples
#' adaptive_strategy_attack <- function(g, attack_strategy, target = "nodes", number = 1)
#Set the environment of the Attack strategy to inside the function so that the correct g is used
adaptive_strategy_attack <- function(g, attack_strategy, target = "nodes", number = 1){
  #g: the netowrk which is being attacked
  #nodes: the number of nodes to Remove
  #seed: The random number generator to use. This is optional
  #Set the environment of the Attack strategy to inside the function so that the correct g is used
  attack_strategy <- set_env(attack_strategy, get_env())
  DeleteVect<- attack_strategy %>%
    eval_tidy
 g_2 <-delete_carefully(g, target, DeleteVect, number)
  return(g_2)
}
