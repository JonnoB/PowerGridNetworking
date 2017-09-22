CreateCascadeGif <- function(CascadeList, VertexNames = "Bus.Name", Folder = NULL, filename = NULL){
  
#creates a GIF from a Cascade list
  #CascadeList:  a list of igraph objects
  #Folder: optional argument specifying Folder path for created files 
  
  #uses the first graph as the reference graph
  g<- CascadeList[[1]]
  
  BaseCoords <- layout_with_fr(CascadeList[[1]]) %>% 
    as_tibble %>% 
    bind_cols(data_frame(Bus.Name = names(V(g))))
  
 CurrentPath <-getwd()
  
  if(is.null(Folder)){
    Folder<-getwd()
  }
  
 if(!is.null(filename)){
   filename <- "Cascade"
 }
 
 
  1:(length(CascadeList))%>% map(~{
  
  PlotGraph <- CascadeList[[.x]]
  E(PlotGraph)$color <- "black"
  
  networkfile <- file.path(Folder, paste0(filename, .x*2 - 1, ".png"))
  png(filename = networkfile )
  
  NetCoords <- data_frame(Bus.Name = names(V(CascadeList[[.x]]))) %>%
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
    networkfile <- paste0(filename, .x*2, ".png")
    
    png(filename=networkfile)
    
    NetCoords <- data_frame(Bus.Name = names(V(CascadeList[[.x]]))) %>%
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


  im.convert(file.path(Folder,
                       paste0(filename, 1:(length(CascadeList)*2-1)), ".png"), 
             output = file.path(Folder,paste0(filename, ".gif")))
  
 #setwd(CurrentPath)
}