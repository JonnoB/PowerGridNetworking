#' matpower Loader
#'
#' This function loads the branch and bus data sections from the matpower dataformat.
#'   The loader has only been tested on the ieee power flow test cases.
#'   together is more efficient than producing each one on its own.
#' @param file_path A character string. The path of the matpower file
#' @param output_graph A logical value. This determines whether the function ouputs an igraph network of the matpower file or a 
#' two element list where the first element is the bus/node data and the second element is the branch data
#' @export
#' @examples
#' # This example requires the loading of a matpower file. I don't know how to add raw datasets to the package yet so this will have to wait.

matpower_loader <- function(file_path, output_graph = TRUE){
  #This function loads the branch and bus data sections from the matpower dataformat.
  #the loader has only been tested on the ieee power flow test cases.
  #file_path a character string. The path of the mapower file
  #output_graph a logical value. This determines whether the function ouputs an igraph network of the matpower file or a 
  #two element list where the first element is the bus/node data and the second element is the branch data
  
  raw_mat <- readLines(file_path) %>%
    as_tibble()
  
  #creates a list of all the parts of the matpower format that are needed for the power grid networking package.
  #This can be expanded if necessary
  mat_list <- c(which(grepl("mpc.bus ", raw_mat$value)), 
                which(grepl("mpc.gen ", raw_mat$value)), 
                which(grepl("mpc.branch ", raw_mat$value)) ) %>% 
    map(~{
      
      #get the column headers
      headers <- raw_mat %>%
        slice(.x-1) %>%
        pull %>%
        sub("%", "", .) %>% trimws %>%
        str_split(., "\\t") %>% .[[1]] #str_split creates a list of vectors there is only a single vector so I take that
      
      #clean the data and separate into tabular format add in the headers
      raw_data <- raw_mat %>%
        filter(1:nrow(.) > .x) %>% #filter all before and including the line declaring the network part data
        filter(1:nrow(.) < which.max(grepl("];", value))) %>% #filter all after the final line of the data
        mutate(value = sub(";", "", value) %>% gsub("%.*", "", .) %>% trimws()) %>%
        separate(data = ., col = value, into = headers, sep = "\\t", convert = TRUE)
      
      return(raw_data)
      
    })
  
  
  bus_data <- left_join(mat_list[[1]], mat_list[[2]], by = c("bus_i"="bus")) %>%
    mutate_all(~ifelse(is.na(.), 0, .)) #non generator nodes have NAs these need to be replaced with 0's
  
  branch_data <- mat_list[[3]]
  
  if(output_graph){
    
    Out <- graph_from_data_frame(d = branch_data, directed = FALSE, vertices = bus_data)
    
  } else {
    
    Out <- list(bus = bus_data, branch = branch_data)
    
  }
  
  return(Out)
  
}