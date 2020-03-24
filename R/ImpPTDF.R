#' Create Impedance matrix and PTDF
#'
#'This is a wrapper for the more interesting parts of the electrical building blocks
#'   It creates both matrices as they almost always both need to be created and they take time so producing
#'   together is more efficient than producing each one on its own.
#' @param g An igraph object representing a power-grid
#' @param SlackRef The slack node for the power-grid, A character vector
#' @param EdgeName the variable that holds the edge names, a character string.
#' @param VertexName the variable that holds the names of the nodes, to identify the slack ref. a character string
#' @export
#' @examples
#' g2 <-make_ego_graph(g, 2, "AXMI")[[1]]
#' ImpPTDF(g, SlackRef)
#This is a reference for the book I used to get good info on the admittance matrix.. Add into package when possible
# @chapter{matlab-program-building,
#   title = "6.4.3 MATLAB Program for Building Ybus Matrix",
#  booktitle = "Power System Analysis (2nd Edition)",
# author = "Nagsarkar, T. K., Sukhija, M. S.",
#   year = "2014",
# isbn = "978-0-19-809633-7",
#   publisher = "Oxford University Press",
#   pages = "232-240"
# }

#This version calculates Azero externally saving quite a lot of time
ImpPTDF <- function(g, SlackRef, AZero, LineProperties, EdgeName = "Link", VertexName = "name", PTDF_only = FALSE){
  #This is a wrapper for the more interesting parts of the electrical building blocks

  #message("Creating Power matrices")
  #edge index is used quite a lot so is assigned here
  edge_index <- get.edge.attribute(g, EdgeName)
  #subset the original edge transmission matrix to only contain current nodes
  AZero <-  AZero[rownames(AZero) %in% edge_index, colnames(AZero) %in% get.vertex.attribute(g, name = VertexName), drop = FALSE]
  #AZero <- CreateTransmission(g, EdgeName, VertexName)
  
  # #remove Slack bus, usually the largest generator
  #drop = FALSE stops the matrix being converted to a vector when there are only two nodes in the sub-graph
  A <- AZero[,colnames(AZero)!=SlackRef, drop = FALSE]

  #Create the diagonal matrix of edge impedance
  C <- LineProperties[edge_index, edge_index] #LinePropertiesMatrix(g, EdgeName, Weight = "Y")

  #message("Inverting the Susceptance matrix") #As this is DC it is the same as the admittance matrix. It is sparse

  B <- t(A) %*% C %*% A
  
  Imp <- base::solve(B) #If the Impedance matrix is inverted again it does not return the original Addmitance matrix due to rounding errors
  #The 0 values of the sparse addmittance matrix are lost and a dense matrix is returned with many very small numbers
  
  #message("Creating the PTDF")
  
  PTDF <- C %*% A %*% Imp


  Out <-list(Imp, PTDF)
  
  names(Out)<- c("Imp", "PTDF")

  return(Out)

}
