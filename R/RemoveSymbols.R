RemoveSymbols <-function(df)  {
  df  %>% mutate_all( funs(gsub("^[^A-Z0-9]{1,}", "", ., ignore.case = FALSE))) %>%
    map_df(parse_guess) 
}