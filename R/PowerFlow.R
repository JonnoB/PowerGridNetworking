#' Calculate DC power flow
#'
#' Calculates the PowerFlow from a graph that contains the following attributes,
#' named edges, edgeweights, a balanced power generation and demand column. Writes a new edge attribute of power flow
#' (existing attribute of the same name will be overwritten).
#' The function outputs a a graph with the correct power flow values
#' @param g An igraph object representing the power grid
#' @param AZero A numeric matrix The transmission matrix of the original network
#' @param LineProperties A numeric matrix. a diagonal matrix of the Y characteristic of the network
#' @param EdgeName The variable that holds the edge names, a character string.
#' @param VertexName The variable that holds the names of the nodes, to identify the slack ref. a character string
#' @param Net_generation The name that the net generation data for each node is held in
#' @param power_flow A character string. The name of the edge attribute that will hold the power flow information
#' @keywords power flow
#'
#' @return A graph with a PowerFlow attribute assigned to the edges
#' @export
#' @examples
#' PowerFlow(g, AZero, LineProperties,
#'EdgeName = "Link",
#'VertexName = "name",
#'Net_generation = "BalencedPower",
#'power_flow = "PowerFlow")

#Azero is calculated externally
PowerFlow <- function(g, AZero, LineProperties,
                       EdgeName = "Link",
                       VertexName = "name",
                       Net_generation = "BalencedPower",
                       power_flow = "PowerFlow"){

  #generate the slack reference bus for each component of the network
  #This creates much smaller matrices which are quicker to invert and operate on.
  slack_ref_df <-  SlackRefFunc(g, VertexName, Generation = Net_generation)

  if(nrow(slack_ref_df)!=0){
    #Calculate power flow for each component of the network as seperate networks
    gList <- 1:nrow(slack_ref_df) %>%
      purrr::map(~{

        #print(paste("PowerFlow for componant", .x))

        SlackRef <- slack_ref_df %>% dplyr::slice(.x)

        gsubset <- igraph::delete.vertices(g, igraph::components(g)$membership != .x)

        if(SlackRef$Nodes > 1){

          #gsubset <- PowerFlow2(gsubset, SlackRef$name, AZero = AZero, LineProperties = LineProperties, EdgeName, VertexName, Net_generation, power_flow)

          InjectionVector <- igraph::get.vertex.attribute(gsubset, name = Net_generation)[igraph::get.vertex.attribute(gsubset, name = VertexName)!=SlackRef$name]

          Power <- ImpPTDF(gsubset,
                           SlackRef = SlackRef$name,
                           AZero = AZero,
                           LineProperties = LineProperties,
                           injection_vector = InjectionVector,
                           EdgeName,
                           VertexName)$Power
          #The Power object is a sparse matrix of dimension 1 will all values will in. this has to be converted to a vector
          #as sometimes there an error is thrown "Error in eattrs[[name]][index] <- value :"
          gsubset <- igraph::set_edge_attr(gsubset, name = power_flow, value = as.vector(Power))
        }

        gsubset

      })

    #create a list of all the edge and vertex attributes for each of the subgraphs
    #Transpose the list and join all edges into a dataframe and all vertices into a dataframe
    gList_2 <- 1:length(gList) %>% map(~{

      igraph::as_data_frame(gList[[.x]], what = "both")

    }) %>%
      purrr::transpose(.) %>%
      map(dplyr::bind_rows)
    #create a graph from the list of length two created in the previous step
    g <- graph_from_data_frame(gList_2$edges, directed = FALSE, vertices = gList_2$vertices)

    #This function could be replaced with the method that just matches edges but it is so fast I don't care enough
    #It would mean that union could be completley removed which woul be more secure and simpler
    # g <- gList %>%
    #   Reduce(union2, .)
  }

  return(g)
}
