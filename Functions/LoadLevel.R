LoadLevel <- function(g){
  df<- data_frame(Line.name = get.edge.attribute(g, name = "name"), 
                  PowerFlow = get.edge.attribute(g, name = "PowerFlow"), 
                  Line.Limit = get.edge.attribute(g, name = "Link.Limit")) %>%
    mutate(LineLoading = abs(PowerFlow)/Line.Limit)
    

  return(df)

}