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
    set.edge.attribute(., "weight", value = get.edge.attribute(., LineLimit)) %>%
    set.vertex.attribute(., "OldStrength", value = strength(.)) %>%
    induced_subgraph(., Nodes) %>%
    set.vertex.attribute(., "StrengthChange", value = strength(.) - get.vertex.attribute(., "OldStrength")) %>%
    set.vertex.attribute(., "Generation",
                         value = get.vertex.attribute(., "Generation") - get.vertex.attribute(., "StrengthChange")) %>%
    BalencedGenDem(., "Demand", "Generation")


}
