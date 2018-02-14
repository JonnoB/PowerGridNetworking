TransferCapacity <- function(g, PTDF){
  
  GenAndDem <-data_frame(
    name = get.vertex.attribute(g, "name"),
    type = case_when(
      get.vertex.attribute(g, "Demand") > get.vertex.attribute(g, "Generation") ~ "Demand",
      get.vertex.attribute(g, "Demand") < get.vertex.attribute(g, "Generation") ~ "Generation",
      TRUE ~"Transmission"
    )) #%>%
    #filter(type != "Transmission")
  

  
  Combos <- combn(1:ncol(PTDF), 2) %>% 
    t %>%
    as.data.frame() %>% 
    mutate(name1 = colnames(PTDF)[V1],
           name2 = colnames(PTDF)[V2],
           type1 = GenAndDem$type[match(name1,GenAndDem$name)],
           type2 = GenAndDem$type[match(name2,GenAndDem$name)],
           calc = ifelse(type1 == "Transmission" | type2 == "Transmission", FALSE, TRUE)) #%>%
   # filter((type1=="Generation" & type2 == "Demand") |(type2=="Generation" & type1 == "Demand"))

  
  LinkLimits <- get.edge.attribute(g, "Link.Limit") %>%
    data_frame(Limits=.)
  
  CapacityVector <-Combos %>% 
    group_by(V1, V2) %>%
    mutate(test = ifelse(calc, min(LinkLimits$Limits/abs(PTDF[,V1] - PTDF[,V2])), 0)) %>%
      ungroup %>% .$test
  
  # CapacityVector <- map2_dbl(.x = Combos[,1], .y = Combos[,2], ~{
  #   LinkLimits2 <-  LinkLimits %>% 
  #     mutate(UnitPower =  abs(PTDF[,.x] - PTDF[,.y]),
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