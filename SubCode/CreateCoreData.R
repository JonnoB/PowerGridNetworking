#Load the onshore Data

setwd(datafile)

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



#Load the OFTO Data



CircuitsOFTO <- AllData[[9]] %>%
  rename(Node1 = Node.1, Node2 =Node.2, Cable.Length..km. = Length..km., Circuit.Type = Type) %>%
  mutate(OHL.Length..km. = 0,
         Winter.Rating..MVA. = Rating..MVA.,
         Spring.Rating..MVA. = Rating..MVA.,
         Summer.Rating..MVA. = Rating..MVA.,
         Autumn.Rating..MVA. = Rating..MVA.,
         Table = "B.2.1d") %>% 
  select(-OFTO, -Station, -Rating..MVA., -Voltage..kV.  ) #voltage ius removed, although it is very useful, for consistancy 

Circuits <- bind_rows(Circuits, CircuitsOFTO)


TransformersOFTO <- AllData[[17]] %>%
  mutate(Table = "B.3.1d") %>% 
  select(-OFTO, -Site, -Rating..MVA.) %>%
  setNames(c("Node1","Node2" ,"R....on.100MVA.","X....on.100MVA.","B....on.100MVA.","Rating..MVA.","Table"  ))


Transformers <- Transformers %>% 
  mutate(Rating..MVA. = as.character(Rating..MVA.)) %>%
  bind_rows(.,TransformersOFTO)

RCEOFTO <- AllData[[25]] %>%
  mutate(MVAr.Generation =  as.numeric(MVAr.Generation),
         MVAr.Absorption =  as.numeric(MVAr.Generation),
         Table = "B.4.1d") %>%
  select(-OFTO,-Tapping)


RCE  <- bind_rows(RCE, RCEOFTO)

rm(list = ls(pattern = "OFTO"))
