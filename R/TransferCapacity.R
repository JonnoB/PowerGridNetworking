TransferCapacity <- function(g, PTDF){

  #Describe the node type
  GenAndDem <-dplyr::data_frame(
    name = igraph::get.vertex.attribute(g, "name"),
    type = dplyr::case_when(
      igraph::get.vertex.attribute(g, "Demand") > igraph::get.vertex.attribute(g, "Generation") ~ "Demand",
      igraph::get.vertex.attribute(g, "Demand") < igraph::get.vertex.attribute(g, "Generation") ~ "Generation",
      TRUE ~"Transmission"
    )) #%>%
    #dplyr::filter(type != "Transmission")


#Find every combination of Demand and Generation pair there should be sum(GenAndDem$type=="Generation") * sum(GenAndDem$type=="Demand") of them
  #Unless the slack node is a demand or generation node then it will be off slightly!
  Combos <- combn(1:ncol(PTDF), 2) %>%
    t %>%
    as.data.frame() %>%
    dplyr::mutate(name1 = colnames(PTDF)[V1],
           name2 = colnames(PTDF)[V2],
           type1 = GenAndDem$type[match(name1,GenAndDem$name)],
           type2 = GenAndDem$type[match(name2,GenAndDem$name)],
           calc = ifelse((type1=="Generation" & type2 == "Demand") |(type2=="Generation" & type1 == "Demand"), TRUE, FALSE))


  LinkLimits <- igraph::get.edge.attribute(g, "Link.Limit") %>%
    dplyr::data_frame(Limits=.)

  CapacityVector <-Combos %>%
    dplyr::group_by(V1, V2) %>%
    dplyr::mutate(test = ifelse(calc, min(LinkLimits$Limits/abs(PTDF[,V1] - PTDF[,V2])), 0)) %>%
      dplyr::ungroup %>% .$test

  # CapacityVector <- purrr::map2_dbl(.x = Combos[,1], .y = Combos[,2], ~{
  #   LinkLimits2 <-  LinkLimits %>%
  #     dplyr::mutate(UnitPower =  abs(PTDF[,.x] - PTDF[,.y]),
  #            MaxPower = Limits/UnitPower)
  #
  #   min(LinkLimits2$MaxPower)
  # })


  CapacityMatrix <- matrix(NA, ncol = ncol(PTDF), nrow = ncol(PTDF))
  CapacityMatrix[Combos[,1]* Combos[,2]] <- CapacityVector

  CapacityMatrix[!is.finite(CapacityMatrix)] <-0

  CapacityMatrix <- CapacityMatrix + t(CapacityMatrix)
 # diag(CapacityMatrix)<- Inf cannot equal inf as this messes up the net-ability score

  return(CapacityMatrix)

}
