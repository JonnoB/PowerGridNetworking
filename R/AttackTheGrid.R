#' Initiate power-grid attack simulation
#'
#' This function simulates an attack on the power grid using the parameter settings you choose
#'    the outut of the function is a nested list of igraph objects.
#' @param NetworkList A list of lists where each element of the sub list is an igraph object, the first time it is used the
#' the network list is simply list(list(g)).
#' @param AttackStrategy A function that calculates which node to delete the function is is in "quo" form and embedded in an
#' attack type.
#' @param referenceGrid The grid that will be used to test the largest component against if NULL it uses the given network.
#' @param MinMaxComp The minimum size of the maximum component, as a fraction, for the process to continue, the default is set
#' to 0.8.
#' @param TotalAttackRounds The maximum number of nodes to be removed before the process stops.
#' @param CascadeMode Whether the power flow equations will be used to check line-overloading or not.
#' @param CumulativeAttacks  The total number of attacks that have taken place so far.
#' @export
#' @examples
#' AttackTheGrid(NetworkList, AttackStrategy, SubstationData, EdgeData, referenceGrid = NULL, MinMaxComp = 0.8)
#' Out <- AttackTheGrid(NetworkList,
#' AttackStrategy,
#' referenceGrid = NULL,
#' MinMaxComp = 0.8,
#' TotalAttackRounds=100,
#' CascadeMode = TRUE,
#' CumulativeAttacks = NULL)

AttackTheGrid <- function(NetworkList,
                          AttackStrategy,
                          referenceGrid = NULL,
                          MinMaxComp = 0.8,
                          TotalAttackRounds=100,
                          CascadeMode = TRUE,
                          CumulativeAttacks = NULL){

  #gets the last network in the list
  gc()
  g <- NetworkList[[length(NetworkList)]]

  g <- g[[length(g)]]

  if(is.null(referenceGrid)){
    referenceGrid  <- g
  }

  #Remove the desired part of the network.
  gCasc <- AttackStrategy %>% #this is crashing... wy?
    eval_tidy(., data = list(g = g)) #The capture environment contains delete nodes, however the current g is fed in here


  #Rebalence network
  #This means that the Cascade calc takes a balanced network which is good.
  gCasc <- BalencedGenDem(gCasc, "Demand", "Generation")

  GridCollapsed <- ecount(gCasc)==0

  gCasc <- list(gCasc)

  if(is.null(CumulativeAttacks)){
    CumulativeAttacks2 <- 1
  } else {
    CumulativeAttacks2 <- CumulativeAttacks + 1
  }

  #This If statement prevents Cascading if theire are no cascadable components
  if(!GridCollapsed){

    if(CascadeMode){
      #this returns a list of networks each of the cascade
      gCasc <- Cascade(gCasc, g0 = g)
    }

    message(paste("Attack ",CumulativeAttacks2, " Nodes Remaining", vcount(gCasc[[length(gCasc)]])))

  } else{

    message("Grid collapsed simulation complete")

  }


  #concatanate the new list with the list of lists
  NetworkList2 <- NetworkList
  NetworkList2[[length(NetworkList2)+1]] <-gCasc

  #extract the last network from the just completed cascade
  gCascLast <- gCasc[[length(gCasc)]]

  #If the largest componant is larger than the MinMaxComp threshold
  #call the function again and delete a new node.

  #when the grid has collapsed problems arise this helps deal with that
  MaxComp <- suppressWarnings(max(components(gCascLast)$csize))

  #Checks to see if the topology of the network is unchanged.
  #If this is TRUE then nothing is being removed and the process can stop
  TopoStability <- (vcount(gCascLast) == vcount(g) &   ecount(gCascLast) == ecount(g))

  FractGC <-ifelse(is.finite(MaxComp),MaxComp/vcount(referenceGrid), 0)

  #These conditions can probably be simplified a bit
  if( !(FractGC < MinMaxComp | length(NetworkList2)-1==TotalAttackRounds| GridCollapsed| TopoStability) ){
    NetworkList2 <- AttackTheGrid(NetworkList = NetworkList2,
                                  AttackStrategy,
                                  referenceGrid = referenceGrid,
                                  MinMaxComp = MinMaxComp,
                                  TotalAttackRounds = TotalAttackRounds,
                                  CascadeMode = CascadeMode,
                                  CumulativeAttacks = CumulativeAttacks2
    )
  }
  gc()
  return(NetworkList2)

}
