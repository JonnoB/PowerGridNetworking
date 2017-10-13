EntropicDegree <- function(g, Scale = TRUE){
  
  ED <- diversity(g)*strength(g)
  
  if(!Scale){
    ED <- ED * log(degree(g))
  }
  
  return(ED)
}