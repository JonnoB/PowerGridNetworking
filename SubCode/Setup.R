packages <- c("tidyverse", "igraph","readr","readxl", "broom", "zoo", "stringr","foreach", "doMC",  "xtable", "geomnet", "ggnetwork", "rlang", "animation", "ggridges")

new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


lapply(packages, library, character.only = TRUE)


select <- dplyr::select

#Set up file system to read the correct folders this switches between aws and windows mode

basewd <- "/home/jonno/Dropbox/Jonathan_Bourne_Phd_Folder"
datafile <- "/home/jonno/Dropbox/Jonathan_Bourne_Phd_Folder/ETYSAppendixB"
LatexFolder <- "/home/jonno/Dropbox/Apps/ShareLaTeX/Method outline" 
FiguresFolder <- file.path(LatexFolder, "Figures")
TablesFolder <- file.path(LatexFolder, "Tables")
MatricesFolder <- file.path(LatexFolder, "Matrices")
Functions <-"/home/jonno/Create_ETYS_network/Functions"
Tariff <- file.path(basewd,"Tariff and Transport")

list.files(Functions) %>% map(~source(file.path(Functions,.x)))

