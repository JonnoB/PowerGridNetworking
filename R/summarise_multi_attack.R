#' Summarise Multi-Attack
#'
#' This function provides a consice way to summarise the output of the extract_attack_stats function, especially when said function
#' has been used over a lot of attacks. This is most common when using "SaveMultiAttacks".
#' @param df A dataframe that is produced by the extract_network_stats function.
#' @param grouping_var The bare unquoted node_name of the grouping variable, the default is alpha
#' @export
#' @examples
#' summarise_multi_attack(df)
summarise_multi_attack <- function(df, grouping_var = alpha){
  grouping_var <- enquo(grouping_var)
 df %>%
    mutate(PGfract = (max(PowerGen)- PowerGen)/max(PowerGen)) %>%
    group_by(NodesAttacked, !!grouping_var) %>%
    summarise(mean = mean(GCfract),
              sd = sd(GCfract),
              GC95 = quantile(GCfract, .95),
              GC05 = quantile(GCfract, .05),
              count = n(),
              mPGfract = mean(PGfract),
              sdPGfract = sd(PGfract),
              medPGfract = median(PGfract),
              PG95 = quantile(PGfract, .95),
              PG05 = quantile(PGfract, .05),
              PGmax = max(PGfract),
              PGmin = min(PGfract),
              mLoad = mean(GridLoading),
              GL95 = quantile(GridLoading, .95),
              GL05 = quantile(GridLoading, .05),
              GC_intact = mean(mean_degree_sqrd>2*mean_degree)) %>%
    ungroup %>%
    group_by(!!grouping_var) %>%
     mutate(ID =n():1) %>%
    ungroup
}
