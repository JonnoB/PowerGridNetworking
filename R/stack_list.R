stack_list <- function(elements)  {
  AllData[elements]%>%
    map2_df(.x =.,
            .y = names(.),
            ~ .x %>% 
              mutate(Table =.y ) %>% remove_symbols
    )}
