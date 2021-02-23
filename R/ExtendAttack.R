#' Extend Attack
#'
#'  Extends a previous attack run with additional attack rounds
#'
#' @param NetworkList The output of AttackTheGrid, which is to be extended
#' @param AttackOrder The original attack order used to generate the attack run
#' @param MinMaxComp The minimum size of the maximum component, default is set to zero
#' @param TotalAttackRounds The total number of attack rounds to execute in this extension
#' @export
#'

ExtendAttack <-  function(NetworkList, AttackOrder, MinMaxComp = 0, TotalAttackRounds = 1000 ){

  g<- NetworkList[[1]][[1]]

  #set strategy
  FixedNodes <- dplyr::quo(FixedStrategyAttack(g, AttackOrder))

  #continue attack
  Fullgraph <- attack_the_grid(g = NetworkList,
                               AttackStrategy = FixedNodes,
                               g0= g,
                               TotalAttackRounds = TotalAttackRounds)

}
