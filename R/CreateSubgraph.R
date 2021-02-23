#' Create Subgraph
#'
#' This function creates a subgraph where the nodes which have lost links to outside the subgraph gain additional generation
#' capacity. This generation capacity is limited to the line limits of the PowerLines.
#' This limit prevents those lines from being overpowered, although it assumes such power is actually available
#'
#' @param g and igraph object
#' @param Nodes A character vector of node names to be in the new graph
#' @param LineLimit The graph attribute name for the line limits of the network
#' @export
#'

CreateSubgraph <- function(g, Nodes, LineLimit = "Link.Limit"){

  g %>%
    igraph::set.edge.attribute(., "weight", value = igraph::get.edge.attribute(., LineLimit)) %>%
    igraph::set.vertex.attribute(., "OldStrength", value = igraph::strength(.)) %>%
    igraph::induced_subgraph(., Nodes) %>%
    igraph::set.vertex.attribute(., "StrengthChange", value = igraph::strength(.) - igraph::get.vertex.attribute(., "OldStrength")) %>%
    igraph::set.vertex.attribute(., "Generation",
                         value = igraph::get.vertex.attribute(., "Generation") - igraph::get.vertex.attribute(., "StrengthChange")) %>%
    BalencedGenDem(., "Demand", "Generation")


}
