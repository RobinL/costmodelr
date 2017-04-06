#' Max of key dates
#'
#' @export
kd_max <- function(key_dates, date_col="date") {
  max(key_dates[[date_col]])
}

#' Min of key dates
#'
#' @export
kd_min <- function(key_dates, date_col="date") {
  min(key_dates[[date_col]])
}


#' Get all dates
#'
#' @export
kd_all_dates_days <- function(key_dates, date_col="date") {
  max_d <- kd_max(key_dates, date_col = date_col)
  min_d <- kd_min(key_dates, date_col = date_col)

  seq(min_d, max_d, by="day")
}

#' Add columns to cost model categorising line items by the additional columns in 'key dates'
#'
#' @export
add_key_dates_categorisations <- function(cost_model) {

  key_dates <- cost_model$key_dates

  all_days <- get_all_dates_df(key_dates)
  all_days <- interpolate_days_character(all_days)

  cost_model$cost_dataframe <- cost_model$cost_dataframe %>%
    dplyr::left_join(all_days, by="date")

  cost_model

}

