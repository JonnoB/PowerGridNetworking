StackList <- function(elements)  {
  AllData[elements]%>%
    purrr::map2_df(.x =.,
            .y = names(.),
            ~ .x %>% 
              dplyr::mutate(Table =.y ) %>% RemoveSymbols
    )}
