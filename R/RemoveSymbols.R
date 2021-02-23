RemoveSymbols <-function(df)  {
  df  %>% dplyr::mutate_all( dplyr::funs(gsub("^[^A-Z0-9]{1,}", "", ., ignore.case = FALSE))) %>%
    purrr::map_df(readr::parse_guess)
}
