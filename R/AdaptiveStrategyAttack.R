#' Make Strategy Adaptive
#'
#' This implements an attack strategy to be adaptive.
#'     This means that it will be re-evaluated every
#'     round to choose the most effective point of attack
#'     according to that metric
#' @param g The network which is being attacked
#' @param AttackStrategy a non-evaluated attack strategy
#' @param Target a character string either Nodes or Edges
#' @param Number The Number of targets to generate
#' @keywords removal regime
#' @export
#' @examples
#' AdaptiveStrategyAttack <- function(g, AttackStrategy, Target = "Nodes", Number = 1)

#Set the environment of the Attack strategy to inside the function so that the correct g is used


AdaptiveStrategyAttack <- function(g, AttackStrategy, Target = "Nodes", Number = 1){

  #Set the environment of the Attack strategy to inside the function so that the correct g is used
  AttackStrategy <- rlang::set_env(AttackStrategy, rlang::get_env())

  DeleteVect<- AttackStrategy %>%
    rlang::eval_tidy

 g2 <-DeleteCarefully(g, Target, DeleteVect, Number)


  return(g2)
}
