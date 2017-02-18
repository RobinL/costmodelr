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
