#' Initiate power-grid attack simulation
#'
#' This function simulates an attack on the power grid using the parameter settings you choose
#'    the outut of the function is a nested list of igraph objects.
#' @param NetworkList a list of lists of networks, when initiating the attack use list(list(g))
#' @param AttackStrategy A function that calculates which node to delete the function is is in "quo" form
#' @param SubstationData Dataframe that contains data on the susbtations
#' @param EdgeData Data frame of Edge data for the network.
#' @param referenceGrid the grid that will be used to test the largest component against if NULL it uses the given network
#' @param MinMaxComp The minimum size of the maximum component for the process to continue
#' @keywords
#' @export
#' @examples
#' AttackTheGrid(NetworkList, AttackStrategy, SubstationData, EdgeData, referenceGrid = NULL, MinMaxComp = 0.8){

AttackTheGrid <- function(NetworkList, AttackStrategy, SubstationData, EdgeData, referenceGrid = NULL, MinMaxComp = 0.8){
  #This function attacks the grid using a given attack strategy

  #gets the last network in the list
  g<- NetworkList[[length(NetworkList)]]

  if(is.null(referenceGrid)){
    referenceGrid  <- g
  }

  deletevertex <- eval_tidy(AttackStrategy)

  #remove selected node from network
  gCasc <- delete.vertices(g, deletevertex)

  #this returns a list of networks each of the cascade
  gCasc <- Cascade(gCasc, SubstationData, EdgeData)

  #If the largest componant is larger than the MinMaxComp threshold
  #call the function again and delete a new node.
  if((max(components(gCasc)$csize)/vcount(referenceGrid)) > MinMaxComp){
    gCasc <- AttackTheGrid(gCasc, AttackStrategy, SubstationData, EdgeData, referenceGrid)
  }

  return(gCasc)

}
