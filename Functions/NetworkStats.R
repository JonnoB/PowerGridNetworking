NetworkStats <- function(g, colname = "value"){
  data_frame(Metric = c("Nodes", "edges", "Components" ,"betweenness", "degree", "assortativity", "clustering", "distance"),
             value = c(vcount(g), ecount(g), components(g)$no, mean(betweenness(g)),mean(degree(g)), assortativity(g, degree(g)),
                       transitivity(g), 1)
  ) %>% setNames(c("Metric", colname))
  
  #This takes forever maybe should replace with *distances* or something else? and custome electrical distance function
  #  mean( ifelse(is.finite(shortest.paths(g)),shortest.paths(g), NA), na.rm = T)
}