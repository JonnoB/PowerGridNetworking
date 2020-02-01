#
# This is a helper function to the union2 function however it doesn't seem to improve performance that much
#I can't really be bothered to test it so it is being kept for some point in the future if necessary
#
CleanNames<- function(g){
  #get names of attributes in all graph parts
  gNames <- list(edge_attr = edge_attr_names(g),
                 graph_attr = graph_attr_names(g),
                 vertex_attr = vertex_attr_names(g))
  print(gNames)
  #identify if the name needs cleaning
  AttrNeedsCleaning <- lapply(gNames,function(n) grepl("(_\\d)$", n ))
  
  #find the stemp of the name
  StemName <- lapply(gNames,function(n) gsub("(_\\d)$", "", n))
  
  #create logical vector of names that will be cleaned. This can possibly be combined with previous step
  NewnNames <- lapply(names(gNames), function(n) unique(StemName[[n]][AttrNeedsCleaning[[n]]]))
  print(NewnNames)
  #name new list
  names(NewnNames) <- names(gNames)
  
  #loop through each element that needs to be renamed
  #this is an unfortunate loop but it doesn't seem possible to do a vectorised version
  #names(edge_attr(g)) <-  gsub("(_\\d)$", "", paste0(names(edge_attr(g)), "_1") ) this works
  #as a renaming method but you overwrite real values with NA
  #first edges
  for( i in NewnNames$edge_attr){
    
    attr1 <- edge_attr(g, paste0(i, "_1"))
    attr2 <- edge_attr(g, paste0(i, "_2"))
    
    g <- set_edge_attr(g, i, value = ifelse(is.na(attr1), attr2, attr1))

    g <- delete_edge_attr(g, paste0(i, "_1"))
    g <- delete_edge_attr(g, paste0(i, "_2"))
    
  } 
  
  for( i in NewnNames$graph_attr){
    
    attr1 <- graph_attr(g, paste0(i, "_1"))
    attr2 <- graph_attr(g, paste0(i, "_2"))
    
    g <- set_graph_attr(g, i, value = ifelse(is.na(attr1), attr2, attr1))
    
    g <- delete_graph_attr(g, paste0(i, "_1"))
    g <- delete_graph_attr(g, paste0(i, "_2"))
    
  }
  
  for( i in NewnNames$vertex_attr){
    
    attr1 <- vertex_attr(g, paste0(i, "_1"))
    attr2 <- vertex_attr(g, paste0(i, "_2"))
    
    g <- set_vertex_attr(g, i, value = ifelse(is.na(attr1), attr2, attr1))
    
    g <- delete_vertex_attr(g, paste0(i, "_1"))
    g <- delete_vertex_attr(g, paste0(i, "_2"))
    
  }
  
  return(g)
  
}



