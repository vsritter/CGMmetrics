#' Title
#'
#' @param file_path
#'
#' @import dplyr
#'
#' @return
#' @export
#'
#' @examples
read_dexcom <- function(file_path) {
  dt <- read.csv(file_path) %>%
    janitor::clean_names()

  # patient summary
  pt <- dt %>%
    filter(event_type %in% c("FirstName", "LastName", "DateOfBirth")) %>%
    select(event_type, patient_info) %>%
    tidyr::pivot_wider(names_from = event_type,
                values_from = patient_info)

  # cgm data
  cgm <- dt %>%
    filter(event_type == 'EGV') %>%
    select(time = timestamp_yyyy_mm_dd_thh_mm_ss,
           glucose = glucose_value_mg_d_l) %>%
    mutate(time = lubridate::ymd_hms(time),
           glucose_low = ifelse(glucose == 'Low', 1, 0),
           glucose_high = ifelse(glucose == 'High', 1, 0),
           glucose = suppressWarnings(as.numeric(glucose)))

  if (nrow(cgm) > 0) {
    range <- cgm %>%
      transmute(date = lubridate::as_date(time)) %>%
      summarise(start_date = min(date),
                end_date = max(date),
                n_days_cgm = length(unique(date)))

    pt <- bind_cols(pt, range)
  }

  return(list(pt = pt, cgm = cgm))
}
