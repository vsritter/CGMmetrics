#' Title
#'
#' @param dt_tir
#' @param months
#'
#' @import dplyr
#' @import ggplot2
#'
#' @return
#' @export
#'
#' @examples
plot_tir <- function(dt_tir, months) {
  dt_tir %>%
    filter(inter %in% months) %>%
    mutate(inter = factor(inter, levels = months),
           bg_level = forcats::fct_rev(bg_level),
           bg_level = forcats::fct_recode(
             bg_level,
             '<54' = '[0,54)',
             '54-69' = '[54,70)',
             '70-180' = '[70,181)',
             '181-250' = '[181,251)',
             '>250' = '[251,Inf]'),
           txt = paste0(round(100*bg_level_dist,1), '%')) %>%
    arrange(inter, desc(bg_level)) %>%
    group_by(inter) %>%
    mutate(cumpct = cumsum(bg_level_dist)) %>%
    ungroup() %>%
    ggplot() +
    geom_col(aes(x = inter, y = bg_level_dist, fill = bg_level), color = 'black') +
    # geom_text(aes(x = inter, y = cumpct, label = txt)) +
    labs(x = 'Time (months)',
         y = '% Glucose Range',
         fill = '') +
    scale_fill_manual(values = c('#fd8d3c', '#fecc5c', '#a6d96a', '#d7191c', '#a6611a')) +
    scale_y_continuous(labels = scales::percent)
}
