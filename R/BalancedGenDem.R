#' Balance Power network
#'
#'Ensures that the demand and generation of the power network are balanced. This is used before power
#'   flow is calculated.
#'
#' @param g An igraph object representing a power network. A character vector.
#' @param  DemandVar The variable in g that contains the demand of each node. A character vector.
#' @param GenerationVar The variable in g that contains the generation of each node. A character vector.
#' @param OutputVar The desired output variable for the balanced net power demand of each node.
#'    A character vector the default is set to "BalencedPower".
#' @export
#' @examples
#' BalencedGenDem(g, DemandVar, GenerationVar, OutputVar = "BalencedPower")
BalancedGenDem <- function(g, DemandVar, GenerationVar, OutputVar = "BalencedPower"){

  #get the components and create the vectors that will be filled with the component generation and demand
  comp_mem <-igraph::components(g)$membership
  sum_dem <- rep(NA, length(comp_mem))
  sum_gen <-  rep(NA, length(comp_mem))

  #Get the nodal demand and generation
  Demand <- igraph::get.vertex.attribute(g, DemandVar)
  Generation <- igraph::get.vertex.attribute(g, GenerationVar)

  #The loop inserts the sum of gen and dem into the respective vector
  for(n in 1:max(comp_mem)){

    sum_dem[comp_mem==n]  <-sum(Demand[comp_mem ==n])
    sum_gen[comp_mem==n]  <-sum(Generation[comp_mem ==n])

  }
  #The dead island is if if either of the vectors is 0 at each element
  #that means that the component lacks generation or demand for the component that node is in.
  DeadIsland <- sum_dem==0 | sum_gen==0

  #If the current demand in the component is greater than the maximum generation
  #reduce the demand proportionally to the level of maximum generation
  Demand <- ifelse(sum_dem>sum_gen, Demand*(sum_gen/sum_dem), Demand)
  #If the current generation is great than the current demand.
  #Proportionally reduce the generation to the level of demand.
  Generation = ifelse(sum_gen>sum_dem, Generation*(sum_dem/sum_gen), Generation)
  #print(all.equal(Demand, df$Demand))

  #Set the output variable to the Total generation minus the total demand for each node, and 0 if it is a dead island.
  g <- igraph::set.vertex.attribute(g, OutputVar, value =(Generation-Demand)*(!DeadIsland))
  #The function can take a binary vector, it doesn't have to be an indexed vector I have checked
  g <- igraph::delete.vertices(g, DeadIsland)
  return(g)


}
