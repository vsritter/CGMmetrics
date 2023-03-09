#' Title
#'
#' @param dt_cgm
#' @param pid
#'
#' @import dplyr
#'
#' @return
#' @export
#'
#' @examples
tir <- function(dt_cgm, pid = 'pid') {
  dt_tir <- dt_cgm_all %>%
    group_by(pid) %>%
    mutate(bg_level = cut(glucose,
                          breaks = c(0, 54, 70, 181, 251, Inf),
                          include.lowest = T, right = F),
           day_nbr = as.numeric(factor(date)),
           inter = cut(day_nbr,
                       breaks = unique(c(seq(1, max(day_nbr), 30), max(day_nbr))),
                       include.lowest = T, right = F, labels = F)) %>%
    ungroup() %>%
    select(pid, inter, time, bg_level, glucose) %>%
    arrange(pid, time)

  # Compute time in range
  dt_tir <- dt_tir %>%
    group_by(pid, inter) %>%
    mutate(row = row_number(),
           change = cumsum(c(1, diff(as.numeric(bg_level)) != 0)),
           time_lead = lead(time)) %>%
    group_by(pid, inter, change) %>%
    summarise(start = min(time), stop = max(time_lead),
              bg_level = unique(bg_level), .groups = 'drop') %>%
    mutate(duration = as.numeric(difftime(stop, start))) %>%
    ungroup()

  dt_tir %>%
    filter(!is.na(duration)) %>%
    group_by(inter, bg_level) %>%
    summarise(bg_level_dist = sum(duration), .groups = 'drop_last') %>%
    mutate(bg_level_dist = bg_level_dist/sum(bg_level_dist)) %>%
    ungroup()
}
