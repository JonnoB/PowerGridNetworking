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

  groupingvar <- dplyr::enquo(groupingvar)

 df %>%
    dplyr::mutate(PGfract = (max(generation)- generation)/max(generation)) %>%
    dplyr::group_by(attack_round, !!groupingvar) %>%
    dplyr::summarise(mean = mean(gc_fract),
              sd = stats::sd(gc_fract),
              GC95 = stats::quantile(gc_fract, .95),
              GC05 = stats::quantile(gc_fract, .05),
              count = dplyr::n(),
              mPGfract = mean(PGfract),
              sdPGfract = stats::sd(PGfract),
              medPGfract = stats::median(PGfract),
              PG95 = stats::quantile(PGfract, .95),
              PG05 = stats::quantile(PGfract, .05),
              PGmax = max(PGfract),
              PGmin = min(PGfract),
              mLoad = mean(mean_loading),
              GL95 = stats::quantile(mean_loading, .95),
              GL05 = stats::quantile(mean_loading, .05),
              GC_intact = mean(mean_degree_sqrd>2*mean_degree)) %>%
    dplyr::ungroup %>%
    dplyr::group_by(!!groupingvar) %>%
     dplyr::mutate(ID =dplyr::n():1) %>%
    dplyr::ungroup

}
