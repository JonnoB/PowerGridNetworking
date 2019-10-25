gg_power_law<- function(df){
  #takes a dataframe of two columns. One column is a numeric that contains the graph metric to be plotted as a 
  #power law graph. the other column names the metric. multiple metrics can be included in a single dataframe
  #and will be plotted with thier own curve.
  #df: data frame with two columns called "values" and "metric"
  #Does not work if the columns don't have the right names!
 PowerInfo <- unique(df$metric) %>%
    map(~{
      df2 <- df %>% filter(metric == .x)
      PL = df2$values %>% conpl$new(.)
      est = estimate_xmin(PL)
      PL$setXmin(est)
     return(PL)
    }) %>% set_names(unique(df$metric))
 PowerDat <- 1:length(PowerInfo) %>%
   map_df(~{
     PowerDat <- plot(PowerInfo[[.x]]) %>% 
       mutate(metric = unique(names(PowerInfo[.x])))
   }) %>%
    rename(Frequency = x, CDF = y)
 FittedLine <- 1:length(PowerInfo) %>%
   map_df(~{
     PowerDat <- lines(PowerInfo[[.x]]) %>% 
       mutate(metric = unique(names(PowerInfo[.x])))
   }) %>%
   rename(Frequency = x, CDF = y)
  PowerDat %>% 
    ggplot(., aes(x = Frequency, y = CDF)) + geom_point(aes(colour = metric)) + 
    scale_x_log10() + scale_y_log10() +
    geom_smooth(data = FittedLine, method = "lm", se = FALSE, aes(linetype = metric)) + 
    annotation_logticks(sides = "lb")
}
