#' Create line limits using a linear model trained on the UK grid
#'
#' These functions set the line limits of network using a model trained on the UK power grid
#' 
#'
#' @param g An igraph object representing a power grid. must have a power flow attribute.
#' @param power_flow The node_name of the graph attribute which contains the power flow over each line.
#' @param voltage The node_name of the graph attribute which contains the voltage. The attribute should be numeric.
#' @param edge_limit The attribute node_name that will be used when setting the line limits.
#' @param node_name Line_limits
#' 
#' @return The original network with an additional edge attribute that is a numeric line limit created by; Line Limit =  \eqn{a*power_flow + b*voltage + c}
#'  
#' @details The models used here were trained on a network representing a simplified version UK high voltage network similar to that used in the 
#'     Electricity Ten Year Statement (ETYS) dataset. The model was validated using ten fold cross validation. In testing the Volt PF was a top 
#'     performer or the best performing model in all tests performed in the paper \href{https://arxiv.org/abs/1907.12848}{Bourne et al. 2019}. 
#'     The PF model generally performed well but not at the level of the Volt_PF model. 
#'     The Volt_PF model was trained using three difference voltage levels 132, 275 and 400Kv.
#'     The relationship between voltage and log10 line limits appeared linear when correcting for power flow. However, we cannot say what will
#'     happen to the relationship at very high or very low voltage levels. The reccomended order in which artificial line limits should be used
#'     given available data is as follows.
#' \enumerate{
#'   \item line_limit_volt_pf
#'   \item line_limit_pf
#'   \item \code{\link{proportional_load}}
#' }
#' 
#'     
#' @seealso \code{\link{proportional_load}} to create proportionally loaded line limits. 
#'   \href{https://arxiv.org/abs/1907.12848}{Bourne et al. 2019} describes these models in detail.
#' @export
#' @examples
#' #Creat line limits using both methods
#' test_g_Volt <- line_limit_volt_pf(g, "power_flow", "Voltage", edge_limit = "Line_Limit_VoltPF")
#' test_g_PF <- line_limit_pf(g, "power_flow", edge_limit = "Line_Limit_PF")
#' #Compare the resulting line limits.
#' tibble(VoltPF = get.edge.attribute(test_g_Volt, "Line_Limit"),
#'     PF =  get.edge.attribute(test_g_PF, "Line_Limit"))
#' #There can be substantial difference between the two due to the additional information provided by the voltage
#'     
#' @rdname edge_limit
line_limit_volt_pf <-function(g, power_flow, voltage, edge_limit = "edge_limit"){
  Power_Flow_vect <- get.edge.attribute(g, power_flow) %>%
    abs(.)
  Voltage_vect <- get.edge.attribute(g, voltage) %>%
    abs(.)
  Line_Limit_vect <- abs(Power_Flow_vect)*0.0002064803 + #Power flow coeff
    Voltage_vect*0.0038424836 + #voltage coeff
    1.8351850106 #Intercept
  g_2 <- set_edge_attr(g, edge_limit, value = 10^Line_Limit_vect) #the model was trained on line limit data that had been transformed using log base 10, this then converts the values back
  return(g_2)
}
#' @export
#' @rdname edge_limit
line_limit_pf <-function(g, power_flow, edge_limit = "edge_limit"){
  Power_Flow_vect <- get.edge.attribute(g, power_flow) %>%
    abs(.)
  Line_Limit_vect <- abs(Power_Flow_vect)*0.0007572902 + #Power flow coeff
    2.7066580789 #Intercept
  g_2 <- set_edge_attr(g, edge_limit, value = 10^Line_Limit_vect) #the model was trained on line limit data that had been transformed using log base 10, this then converts the values back
  return(g_2)
}
