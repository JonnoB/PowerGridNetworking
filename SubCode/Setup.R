packages <- c("tidyverse", "igraph","readr","readxl", "broom", "zoo", "stringr", "xtable")

new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


lapply(packages, library, character.only = TRUE)


select <- dplyr::select

#Set up file system to read the correct folders this switches between aws and windows mode

basewd<- "/home/jonno/Dropbox/Jonathan Bourne Phd Folder"
datafile<- "/home/jonno/Dropbox/Jonathan Bourne Phd Folder/ETYSAppendixB"
Functions <-"/home/jonno/Create_ETYS_network/Functions"
Tariff <- file.path(basewd,"Tariff and Transport")

list.files(Functions) %>% map(~source(file.path(Functions,.x)))

