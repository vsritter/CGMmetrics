#' Title
#'
#' @param dt_cgm
#' @param lookback
#' @param rate
#'
#' @import dplyr
#'
#' @return
#' @export
#'
#' @examples
calc_gmi <- function(dt_cgm, lookback = 90, rate = 14) {
  cgm <- dt_cgm %>%
    transmute(
      date = lubridate::as_date(time),
      glucose = ifelse(glucose_low == 1, min(glucose, na.rm = T), glucose),
      glucose = ifelse(glucose_high == 1, max(glucose, na.rm = T), glucose))

  dt_gmi <- data.frame(
    dt_gmi = seq.Date(min(cgm[['date']]), max(cgm[['date']]), by = '14 day')[-1]) %>%
    rowwise() %>%
    mutate(
      avg_glucose = filter(cgm, date >= dt_gmi-90, date < dt_gmi) %>% with(glucose) %>% mean(),
      n_cgm_days = filter(cgm, date >= dt_gmi-90, date < dt_gmi) %>% with(date) %>% unique() %>% length(),
      gmi = 3.31 + 0.02392*avg_glucose) %>%
    ungroup() %>%
    filter(n_cgm_days >= 10)
}
