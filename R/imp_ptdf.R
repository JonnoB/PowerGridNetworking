#' Create Impedance matrix and ptdf
#'
#'This is a wrapper for the more interesting parts of the electrical building blocks
#'   It creates both matrices as they almost always both need to be created and they take time so producing
#'   together is more efficient than producing each one on its own.
#' @param g An igraph object representing a power-grid
#' @param slack_ref The slack node for the power-grid, A character vector
#' @param edge_name the variable that holds the edge names, a character string.
#' @param node_name the variable that holds the names of the nodes, to identify the slack ref. a character string
#' @export
#' @examples
#' g_2 <-make_ego_graph(g, 2, "AXMI")[[1]]
#' imp_ptdf(g, slack_ref)
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
imp_ptdf <- function(g, slack_ref, edge_name = "edge_name", node_name = "name"){
  #This is a wrapper for the more interesting parts of the electrical building blocks
  message("Creating Power matrices")
  AZero <- create_transmission(g, edge_name, node_name)
  # #remove Slack bus, usually the largest generator
  #drop = FALSE stops the matrix being converted to a vector when there are only two nodes in the sub-graph
  A <- AZero[,colnames(AZero)!=slack_ref, drop = FALSE]
  #Create the diagonal matrix of edge impedance
  C <- line_properties_matrix(g)
  B <- t(A) %*% C %*% A
  message("Inverting the Susceptance matrix") #As this is DC it is the same as the admittance matrix. It is sparse
  imp <- solve(B) #If the Impedance matrix is inverted again it does not return the original Addmitance matrix due to rounding errors
  #The 0 values of the sparse addmittance matrix are lost and a dense matrix is returned with many very small numbers
  message("Creating the PTDF")
  ptdf <- C %*% A %*% imp
  Out <-list(imp, ptdf)
  names(Out)<- c("Imp", "PTDF")
  return(Out)
}
