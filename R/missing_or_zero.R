#' Missing or Zero replacement
#' 
#' I have no idea if function is used or needs to be deleted.
#' @param data A vector
#' @param type A character string. either max or min
#' @export
#' @examples 
#' missing_or_zero(data, type)

missing_or_zero <- function(data, type){
  #replaces missing or zero values with the lowest or highest value of the vector
  #data: the vector that needs replacing
  #type = max or min enetered as a character vector
  funct <- paste0(type, "(data[data!=0],na.rm=T)")
  ifelse(data==0| is.na(data),
         eval(parse(text = funct)),
         data
  )
}           
