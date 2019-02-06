#' Extracts Attack stats from saved
#'
#' Extracts the attack statistics for each network in a folder and combines it into a single file that is also saved.
#' This function only works on a folder of folders...It needs to be changed
#'
#' @param RootFolder :The folder where the attack files are saved
#' @param NewFolderPath : The folder where the summary files will be saved to
#' @export

ExtractAttackStats<- function(RootFolder, NewfolderPath ){

  list.files(RootFolder , full.names = TRUE) %>%
    walk(~{

      rootfolder <- .x
      targetfolder <- basename(.x)
      savename <-paste0(targetfolder, ".rds") %>% file.path(NewfolderPath,.)
      print(savename)

      #Create directory if needed
      if(!file.exists(NewfolderPath)){
        dir.create(NewfolderPath)
      }

      if(!file.exists(savename)){

        print(paste("Extracting summary data for", targetfolder))

        summarydata <-list.files(rootfolder) %>%
          map_df(~{
            print(.x)
            read_rds(file.path(rootfolder, .x)) %>%
              ExtractNetworkStats()%>%
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
