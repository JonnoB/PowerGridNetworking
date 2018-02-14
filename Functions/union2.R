union2<-function(g1, g2){
  #Creates the union of two networks, and merges attributes with the same name.
  #In the case where there are overlapping nodes the attributes of g1 take precedence
  #g1 & g2: Igraph networks which should be merged.
  
  g <- union(g1, g2)

#Looks to see which attributes need cleaning
CleanEdgeAttr <- get.edge.attribute(g) %>% names() %>% grepl("(_\\d)$", . )

EdgeNames  <- get.edge.attribute(g) %>% names() %>% gsub("(_\\d)$", "", .)

#Looks to see which attributes need cleaning
CleanVertexAttr <- get.vertex.attribute(g) %>% names() %>% grepl("(_\\d)$", . )

VertexNames  <- get.vertex.attribute(g) %>% names() %>% gsub("(_\\d)$", "", .)


#Clean up Edges
for( i in unique(EdgeNames[CleanEdgeAttr])){
  
  attr1 <- get.edge.attribute(g, paste0(i, "_1")) 
  attr2 <- get.edge.attribute(g, paste0(i, "_2"))
  
  g <- set.edge.attribute(g, i, value = ifelse(is.na(attr1), attr2, attr1))
  
  g <- remove.edge.attribute(g, paste0(i, "_1"))
  g <- remove.edge.attribute(g, paste0(i, "_2"))
  
}

#Clean up vertices
for( i in unique(VertexNames[CleanVertexAttr])){
  
  attr1 <- get.vertex.attribute(g, paste0(i, "_1")) 
  attr2 <- get.vertex.attribute(g, paste0(i, "_2"))
  
  g <- set.vertex.attribute(g, i, value = ifelse(is.na(attr1), attr2, attr1))
  
  g <- remove.vertex.attribute(g, paste0(i, "_1"))
  g <- remove.vertex.attribute(g, paste0(i, "_2"))
  
}

return(g)

}