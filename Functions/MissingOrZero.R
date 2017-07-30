MissingOrZero <- function(data, type){
  #replaces missing or zero values with the lowest or highest value of the vector
  #data: the vector that needs replacing
  #type = max or min enetered as a character vector
  funct <- paste0(type, "(data[data!=0],na.rm=T)")
  
  ifelse(data==0| is.na(data),
         eval(parse(text = funct)),
         data
  )
  
}           
