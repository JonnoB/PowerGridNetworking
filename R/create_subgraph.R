#' Create Subgraph
#'
#' This function creates a subgraph where the nodes which have lost links to outside the subgraph gain additional generation
#' capacity. This generation capacity is limited to the line limits of the PowerLines.
#' This limit prevents those lines from being overpowered, although it assumes such power is actually available
#'
#' @param g and igraph object
#' @param nodes A character vector of node names to be in the new graph
#' @param edge_limit The graph attribute node_name for the line limits of the network
#' @export
#'
create_subgraph <- function(g, nodes, edge_limit = "edge_limit"){
  g %>%
    set.edge.attribute(., "weight", value = get.edge.attribute(., edge_limit)) %>%
    set.vertex.attribute(., "OldStrength", value = strength(.)) %>%
    induced_subgraph(., nodes) %>%
    set.vertex.attribute(., "StrengthChange", value = strength(.) - get.vertex.attribute(., "OldStrength")) %>%
    set.vertex.attribute(., "generation",
                         value = get.vertex.attribute(., "generation") - get.vertex.attribute(., "StrengthChange")) %>%
    balance_generation_and_demand(., "demand", "generation")
}
