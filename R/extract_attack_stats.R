#' Extracts Attack stats from saved
#'
#' Extracts the attack statistics for each network in a folder and combines it into a single file that is also saved.
#' This function only works on a folder of folders...It needs to be changed
#'
#' @param root_folder :The folder where the attack files are saved
#' @param NewFolderPath : The folder where the summary files will be saved to
#' @param generation The node_name of the variable that stores the net generation data. character string
#' @param edge_name A character string. The default is "name".
#' @param power_flow A character String. The default is "power_flow".
#' @param edge_limit A character string. The default is "edge_limit".
#' @export
extract_attack_stats<- function(root_folder, 
                              newfolder_path, 
                              generation = "net_generation", 
                              edge_name = "name", 
                              power_flow = "power_flow", 
                              edge_limit = "edge_limit" ){
  list.files(root_folder , full.names = TRUE) %>%
    walk(~{
      rootfolder <- .x
      targetfolder <- basename(.x)
      savename <-paste0(targetfolder, ".rds") %>% file.path(newfolder_path,.)
      print(savename)
      #Create directory if needed
      if(!file.exists(newfolder_path)){
        dir.create(newfolder_path)
      }
      if(!file.exists(savename)){
        print(paste("Extracting summary data for", targetfolder))
        summarydata <-list.files(rootfolder) %>%
          map_df(~{
            print(.x)
            read_rds(file.path(rootfolder, .x)) %>%
              extract_network_stats(generation = generation, edge_name = edge_name, power_flow = power_flow, edge_limit = edge_limit)%>%
              mutate( simulationID = gsub("\\.rds", "", .x ) %>% gsub("Simulation_ID_", "", .) %>% as.integer)
          }
          ) %>%
          mutate(alpha = targetfolder,
                 GridLoading = ifelse(Blackout==1, 0, GridLoading))
        saveRDS(summarydata, savename)
        print(paste("File", savename, "saved"))
      } else {print(paste("saved file for" , targetfolder, "exists"))}
    }
    )
}
