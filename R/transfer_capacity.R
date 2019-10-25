#' Transfer capacity
#' 
#' This function is an intermediate step in the calculation of net-ability
#' 
#' @param g An igraph object
#' @param ptdf a matrix. This is the output of the imp_ptdf function
#' @export
#' @seealso \code{\link{net_ability}}
#' @examples 
#' transfer_capacity(g, ptdf)

transfer_capacity <- function(g, ptdf){
  #Describe the node type
  GenAndDem <-data_frame(
    node_name = get.vertex.attribute(g, "name"),
    type = case_when(
      get.vertex.attribute(g, "demand") > get.vertex.attribute(g, "generation") ~ "demand",
      get.vertex.attribute(g, "demand") < get.vertex.attribute(g, "generation") ~ "generation",
      TRUE ~"Transmission"
    )) #%>%
    #filter(type != "Transmission")
#Find every combination of demand and generation pair there should be sum(GenAndDem$type=="generation") * sum(GenAndDem$type=="demand") of them
  #Unless the slack node is a demand or generation node then it will be off slightly!
  Combos <- combn(1:ncol(ptdf), 2) %>%
    t %>%
    as.data.frame() %>%
    mutate(node_name1 = colnames(ptdf)[V1],
           node_name2 = colnames(ptdf)[V2],
           type1 = GenAndDem$type[match(node_name1,GenAndDem$node_name)],
           type2 = GenAndDem$type[match(node_name2,GenAndDem$node_name)],
           calc = ifelse((type1=="generation" & type2 == "demand") |(type2=="generation" & type1 == "demand"), TRUE, FALSE))
  LinkLimits <- get.edge.attribute(g, "edge_limit") %>%
    data_frame(Limits=.)
  CapacityVector <-Combos %>%
    group_by(V1, V2) %>%
    mutate(test = ifelse(calc, min(LinkLimits$Limits/abs(ptdf[,V1] - ptdf[,V2])), 0)) %>%
      ungroup %>% .$test
  # CapacityVector <- map2_dbl(.x = Combos[,1], .y = Combos[,2], ~{
  #   LinkLimits2 <-  LinkLimits %>%
  #     mutate(UnitPower =  abs(ptdf[,.x] - ptdf[,.y]),
  #            MaxPower = Limits/UnitPower)
  #
  #   min(LinkLimits2$MaxPower)
  # })
  CapacityMatrix <- matrix(NA, ncol = ncol(ptdf), nrow = ncol(ptdf))
  CapacityMatrix[Combos[,1]* Combos[,2]] <- CapacityVector
  CapacityMatrix[!is.finite(CapacityMatrix)] <-0
  CapacityMatrix <- CapacityMatrix + t(CapacityMatrix)
 # diag(CapacityMatrix)<- Inf cannot equal inf as this messes up the net-ability score
  return(CapacityMatrix)
}
