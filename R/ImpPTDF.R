#' Create Impedance matrix and PTDF
#'
#'This is a wrapper for the more interesting parts of the electrical building blocks
#'   It creates both matrices as they almost always both need to be created and they take time so producing
#'   together is more efficient than producing each one on its own.
#' @param g An igraph object representing a power-grid
#' @param SlackRef The slack node for the power-grid, A character vector
#' @export
#' @example
#' ImpPTDF(g, SlackRef)
ImpPTDF <- function(g, SlackRef){
  #This is a wrapper for the more interesting parts of the electrical building blocks

  message("Creating Power matrices")

  AZero <- CreateTransmission(g, "Link")

  # #remove Slack bus, usually the largest generator
  #drop = FALSE stops the matrix being converted to a vector when there are only two nodes in the sub-graph
  A <- AZero[,colnames(AZero)!=SlackRef, drop = FALSE]

  #Create the diagonal matrix of edge to itself impedance
  C <- LinePropertiesMatrix(g)

  B <- t(A) %*% C %*% A

  message("Inverting the Susceptance matrix")

  Imp <- solve(B)

  message("Creating the PTDF")

  PTDF <- C %*% A %*% Imp

  Out <-list(Imp, PTDF)

  names(Out)<- c("Imp", "PTDF")

  return(Out)

}
