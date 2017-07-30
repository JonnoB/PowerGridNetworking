BalencedGenDem <- function(Graph, InjectionVector, NodeNames = NULL){
  #This function creates teh balenced supply and generation vector and can be subsetted to a group of specific nodes.
  
  df <- data_frame(Site = names(components(Graph)$membership) %>% unlist,
                   Componant = components(Graph)$membership %>% unlist) 
  
  if(!is.null(NodeNames)){ 
    df <- df%>%
      filter(Site %in% NodeNames)
  }
  Out <- df %>% left_join(InjectionVector, by ="Site") %>%
    group_by(Componant) %>%
    mutate(TotMWDem = sum(MWDem),
           TotMWGen = sum(MWGen),
           diff = ifelse(TotMWDem==0 | TotMWGen==0,0,TotMWDem/TotMWGen),
           AdjNetPower = diff*MWGen-MWDem)
  
  return(Out)
}