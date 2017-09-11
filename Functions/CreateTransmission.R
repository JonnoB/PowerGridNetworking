CreateTransmission <- function(g, Edgename){
  #his function creates the transmission matrix between the edges and the In and Out Nodes
  #It follows the method set out in Pepyne 2007
  #It takes in a graph and outputs a numeric matrix which has -1,0-1 as values.
  
  #g: a network with attribute of named edges. This is an igraph object
  #Edgename: the attribute name of the edges, this is a character vector
  
  #Create the edge list
  Edgelist <- get.edgelist(g) %>%
    as_tibble %>%
    mutate(Link = get.edge.attribute(g, Edgename))
  
  #Produce the In and out dataframes
  
  In <- Edgelist %>%
    mutate(
      name = V1,
      Adjacent = 1) %>%
    select(-V1,-V2)
  
  Out <- Edgelist %>%
    mutate(
      name = V2,
      Adjacent = -1) %>%
    select(-V1,-V2)
  
  Transdf <- bind_rows(In, Out) %>%
    spread(., key = name, value = Adjacent, fill = 0)
  
  #Convert to a matrix
  Transmat <- Transdf %>%
    select(-Link) %>%
    as.matrix
  
  #name the rows.
  rownames(Transmat) <- Transdf$Link
  
  #Reorder the rows and columns so they are in the same order
  #head(rownames(Transmat)); head(get.edge.attribute(g, "Link"))
  #head(colnames(Transmat)); head(get.vertex.attribute(g, "name"))

    
  Transmat <- Transmat[match(get.edge.attribute(g, "Link"), rownames(Transmat)),  
                       match(get.vertex.attribute(g, "name"), colnames(Transmat)) ]
    
  return(Transmat)
}