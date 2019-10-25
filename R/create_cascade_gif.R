create_cascade_gif <- function(CascadeList, VertexNames = "Bus.Name", Folder = NULL, filename = NULL){
#creates a GIF from a cascade list
    #CascadeList:  a list of of igraph objects can also be a list of lists aka the out put of attack_the_grid
    #Vertextnames: a character string giving the igraph vertex attribute that is  node_name of the vertices
    #Folder: optional argument specifying Folder path for created files. if NULL working directory used
      #The folder path cannot have any spaces in it!
    #filename: opttional, if left blank "Cascade" is used
  #flatten the list, is type safe so the igraph objects don't get flattened
  CascadeList <- squash(CascadeList)
  #uses the first graph as the reference graph
  g<- CascadeList[[1]]
  BaseCoords <- layout_with_fr(CascadeList[[1]]) %>% 
    as_tibble %>% 
    bind_cols(data_frame(Bus.node_name = names(V(g))))
 CurrentPath <-getwd()
  if(is.null(Folder)){
    Folder<-getwd()
  }
 if(is.null(filename)){
   filename <- "Cascade"
 }
  1:(length(CascadeList))%>% map(~{
  PlotGraph <- CascadeList[[.x]]
  E(PlotGraph)$color <- "black"
  networkfile <- file.path(Folder, paste0(filename, .x*2 - 1, ".png"))
  png(filename = networkfile )
  NetCoords <- data_frame(Bus.node_name = names(V(CascadeList[[.x]]))) %>%
    left_join(BaseCoords, by= VertexNames)
  PlotGraph %>% 
    plot(.,
         layout = as.matrix(NetCoords[,2:3]),
         vertex.size=.8, 
         edge.arrow.size=.4, 
         vertex.label = NA, 
         rescale = FALSE,
         xlim = c(min(BaseCoords[,1]), max(BaseCoords[,1])),
         ylim = c(min(BaseCoords[,2]), max(BaseCoords[,2]))
    )
  dev.off()    
  if(.x < length(CascadeList)){  
    networkfile <-  file.path(Folder, paste0(filename, .x*2, ".png"))
    png(filename=networkfile)
    NetCoords <- data_frame(Bus.node_name = names(V(CascadeList[[.x]]))) %>%
      left_join(BaseCoords, by= VertexNames)
    PlotGraph <- ColourDeletedEdges(CascadeList[[.x]], CascadeList[[.x+1]])
    PlotGraph %>% 
      plot(.,
           layout = as.matrix(NetCoords[,2:3]),
           vertex.size=.8, 
           edge.arrow.size=.4, 
           vertex.label = NA, 
           rescale = FALSE,
           xlim = c(min(BaseCoords[,1]), max(BaseCoords[,1])),
           ylim = c(min(BaseCoords[,2]), max(BaseCoords[,2]))
      )
    dev.off()
  }
})
LoadFiles <- file.path(Folder,
                       paste0(filename, 1:(length(CascadeList)*2-1), ".png"))
  im.convert(LoadFiles, output = file.path(Folder,paste0(filename, ".gif")))
 #setwd(CurrentPath)
}
