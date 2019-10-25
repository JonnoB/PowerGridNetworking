#' remove_symbols
#' 
#' probably shouldn't be in this package. I need to chek the dependencies before removing
#' @param df a dataframe
#' @export


remove_symbols <-function(df)  {
  df  %>% mutate_all( funs(gsub("^[^A-Z0-9]{1,}", "", ., ignore.case = FALSE))) %>%
    map_df(parse_guess) 
}
