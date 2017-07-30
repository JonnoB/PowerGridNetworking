#This Script loads the data from the Appendix B, Appendix F and the generation sheets.

KeepPath <- getwd()

#create the path to the file
path <- file.path(datafile, "ETYS 2016 Appendix B.xlsx")


#Create a list of all the sheets in the file

AllData <- path %>% 
  excel_sheets(.) %>%
  lapply(., read_excel, path = path, skip = 1, trim_ws = TRUE ) %>%
  map(~.x %>% setNames(make.names(names(.))))

#break out the sheet names

AllData[[1]] <- AllData[[1]] %>% 
  setNames(c("Table", "Title")) %>% 
  filter(complete.cases(.)) %>%
  mutate(Table = replace(Table, Table=="Table", "Index"), 
         element = 1:(n())) #provides list element reference

names(AllData) <- AllData[[1]]$Table



#Load the Appendix B data

Substation <- StackList(2:4) %>%
  mutate(Site.Code = str_trim(Site.Code),
         Site.Name = str_trim(Site.Name)) #remove leading and trailing whitespace which causes problems with analysis.

Circuits <- StackList(6:8)

names(AllData[[16]])[1:2] <-c("Node1", "Node2") #The name had been written differently to the other two
Transformers <- StackList(14:16)

#The RCE needs a lot of treatment as there are formatting differences and also the bind_rows part of map_df doesn't coerce, which is annoying.
names(AllData[[24]])[4] <-"MVAr.Generation"
names(AllData[[23]])[5] <-"MVAr.Absorption"
names(AllData[[24]])[5] <-"MVAr.Absorption"

AllData[[22]]$MVAr.Absorption<- AllData[[22]]$MVAr.Absorption %>%gsub("[^A-Z0-9]", "", ., ignore.case = TRUE)


RCE<- AllData[22:24] %>% map(~.x %>% RemoveSymbols %>% 
                               mutate(Unit.Number = as.character(Unit.Number),
                                      Connection.Voltage..kV. = as.character(Connection.Voltage..kV.)))%>%
  map2_df(.x =.,
          .y = names(.),
          ~ .x %>% 
            mutate(Table =.y ) )


#Load Appendix F


DemandData <- read_excel(file.path(datafile,"ETYS16 AppG demand data.xlsx"), skip = 7) %>%
  mutate(Site = str_sub(Node,1,4) )


#Load Tariff aka generation data
setwd(Tariff)

GenerationData <- read_excel("Tariff & Transport Model_2018_19 Tariffs_External.xlsm", sheet =10, skip = 33) %>%
  setNames(make.names(names(.))) %>%
  mutate(Site = str_sub(Node.1,1,4))

LocalAssetData <- read_excel("Tariff & Transport Model_2018_19 Tariffs_External.xlsm", sheet =11, skip = 11) %>%
  setNames(make.names(names(.)))

TransportData <- read_excel("Tariff & Transport Model_2018_19 Tariffs_External.xlsm", sheet =12, skip = 11) %>%
  setNames(make.names(names(.)))

setwd(KeepPath)
rm(KeepPath)