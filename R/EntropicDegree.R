EntropicDegree <- function(g, value = "Link.Limit", Scale = FALSE){
  
  E(g)$weight <- get.edge.attribute(g, name = value)
  
  ED <- (1-diversity(g))*strength(g)
  
  if(!Scale){
    ED <- ED * log(degree(g))
  }
  
  return(ED)
}