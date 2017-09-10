CreateCascadeGif <- function(CascadeList, Folder = NULL, filename = NULL){
  
#creates a GIF from a Cascade list
  #CascadeList:  a list of igraph objects
  #Folder: optional argument specifying Folder path for created files 
  
  BaseCoords <- layout_with_fr(CascadeList[[1]]) %>% 
    as_tibble %>% 
    bind_cols(data_frame(Bus.Name = names(V(gbase))))
  
 CurrentPath <-getwd()
  
  if(!is.null(Folder)){
    setwd(Folder)
  }
  
 if(!is.null(filename)){
   filename <- "Cascade"
 }
 
 
  1:(length(CascadeList))%>% map(~{
  
  PlotGraph <- CascadeList[[.x]]
  E(PlotGraph)$color <- "black"
  
  networkfile <- paste0(filename, .x*2 - 1, ".png")
  png(filename = networkfile )
  
  NetCoords <- data_frame(Bus.Name = names(V(CascadeList[[.x]]))) %>%
    left_join(BaseCoords, by= "Bus.Name")
  
  PlotGraph %>% 
    plot(.,
         layout = as.matrix(NetCoords[,2:3]),
         vertex.size=.8, 
         edge.arrow.size=.4, 
         vertex.label = NA
    )
  
  dev.off()    
  
  
  if(.x < length(CascadeList)){  
    networkfile <- paste0(filename, .x*2, ".png")
    
    png(filename=networkfile)
    
    NetCoords <- data_frame(Bus.Name = names(V(CascadeList[[.x]]))) %>%
      left_join(BaseCoords, by= "Bus.Name")
    
    
    PlotGraph <- ColourDeletedEdges(CascadeList[[.x]], CascadeList[[.x+1]])
    
    PlotGraph %>% 
      plot(.,
           layout = as.matrix(NetCoords[,2:3]),
           vertex.size=.8, 
           edge.arrow.size=.4, 
           vertex.label = NA
      )
    
    dev.off()
    
  }
  
  
})


  im.convert(paste0(filename, 1:(length(CascadeList)*2-1), ".png"), output = paste0(filename, ".gif"))
  
  setwd(CurrentPath)
}