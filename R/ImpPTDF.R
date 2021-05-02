#' Create Impedance matrix and PTDF
#'
#'This is a wrapper for the more interesting parts of the electrical building blocks
#'   It creates both matrices as they almost always both need to be created and they take time so producing
#'   together is more efficient than producing each one on its own.
#' @param g An igraph object representing a power-grid
#' @param SlackRef The slack node for the power-grid, A character vector
#' @param AZero a matrix. This is created by the 'CreateTransmission' function
#' @param LineProperties a matrix This is created by the LinePropertiesMatrix function
#' @param injection_vector a numeric vector. The net generation values for all the nodes in the network
#' @param EdgeName the variable that holds the edge names, a character string.
#' @param VertexName the variable that holds the names of the nodes, to identify the slack ref. a character string
#' to increase speed
#'
#' @details This function calculates the PTDF and the impedance matrix also the power flow across the edges. However,
#' the function outputs either the power flow or the PTDF and the impedance matrix. This is because by outputting only
#' the power flow the calculation can be made much faster by avoiding matrix inversion. This is especially important
#' as the matrices are sparse and inversion would create a dense matrix which is much larger in terms of RAM and slower in terms of
#' processing speed. When the PTDF and impedance matrix are explicity required, then the injection_vector variable should be NULL
#'
#' @export
#' @seealso \code{\link{attack_the_grid}},\code{CreateTransmission}, \code{LinePropertiesMatrix}
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
ImpPTDF <- function(g, SlackRef, AZero, LineProperties, injection_vector = NULL ,EdgeName = "Link", VertexName = "name"){

  #edge index is used several times in the function so is assigned here to save time
  edge_index <- igraph::get.edge.attribute(g, EdgeName)
  #subset the original edge transmission matrix to only contain current nodes
  AZero <-  AZero[rownames(AZero) %in% edge_index, colnames(AZero) %in% igraph::get.vertex.attribute(g, name = VertexName), drop = FALSE]

  #Remove Slack bus, usually the largest generator
  #drop = FALSE stops the matrix being converted to a vector when there are only two nodes in the sub-graph
  A <- AZero[,colnames(AZero)!=SlackRef, drop = FALSE]

  #Subset the line properties matrix to only the relevant edges
  C <- LineProperties[rownames(LineProperties) %in% edge_index, colnames(LineProperties) %in% edge_index]

  B <- Matrix::t(A) %*% C %*% A

  if(is.null(injection_vector)){

    Imp <- Matrix::solve(B) #If the Impedance matrix is inverted again it does not return the original Admittance matrix due to rounding errors
    #The 0 values of the sparse admittance matrix are lost and a dense matrix is returned with many very small numbers

    PTDF <- C %*% A %*% Imp

    Out <-list(Imp, PTDF, NULL)

  } else {

    #this creates a temporary vector avoiding matrix inversion
    #Which is much faster when only the power flow is needed, it also uses much less RAM
    temp <- Matrix::solve(B, injection_vector)

    Power <- C %*% A %*% temp

    Out <-list(NULL, NULL, Power)

  }


  names(Out)<- c("Imp", "PTDF", "Power")

  return(Out)

}
