#' Summarise Multi-Attack
#'
#' This function provides a concise way to summarise the output of the ExtractAttackStats function, especially when said function
#' has been used over a lot of attacks. This is most common when using "SaveMultiAttacks".
#' @param df A dataframe that is produced by the ExtractNetworkStats function.
#' @param groupingvar The bare unquoted name of the grouping variable, the default is alpha
#' @export
#' @examples
#' SummariseMultiAttack(df)

SummariseMultiAttack <- function(df, groupingvar = alpha){

  groupingvar <- enquo(groupingvar)

 df %>%
    mutate(PGfract = (max(generation)- generation)/max(generation)) %>%
    group_by(attack_round, !!groupingvar) %>%
    summarise(mean = mean(gc_fract),
              sd = sd(gc_fract),
              GC95 = quantile(gc_fract, .95),
              GC05 = quantile(gc_fract, .05),
              count = n(),
              mPGfract = mean(PGfract),
              sdPGfract = sd(PGfract),
              medPGfract = median(PGfract),
              PG95 = quantile(PGfract, .95),
              PG05 = quantile(PGfract, .05),
              PGmax = max(PGfract),
              PGmin = min(PGfract),
              mLoad = mean(mean_loading),
              GL95 = quantile(mean_loading, .95),
              GL05 = quantile(mean_loading, .05),
              GC_intact = mean(mean_degree_sqrd>2*mean_degree)) %>%
    ungroup %>%
    group_by(!!groupingvar) %>%
     mutate(ID =n():1) %>%
    ungroup

}
