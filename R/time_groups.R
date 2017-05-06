date_col_week <- function(date_vector) {
  format(date_vector, "Week %W %Y")
}

date_col_month <- function(date_vector) {
  format(date_vector, "%b %Y")
}

date_col_year <- function(date_vector) {
  format(date_vector, "%Y")
}

date_col_quarter <- function(date_vector) {
  month <- as.double(format(date_vector, "%m"))
  quarter <- ceiling((month-0.1)/3)
  year <- format(date_vector, "%Y")
  paste0(year, "-Q", quarter)
}

date_col_fy <- function(date_vector) {

  year <- as.double(format(date_vector, "%y"))
  month <- as.double(format(date_vector, "%m"))
  quarter <- ceiling((month-0.1)/3)
  qsplit <- ifelse(quarter==1, 0, 1)
  paste0("FY", year+qsplit -1, "/", year+qsplit)

}

date_col_fy_quarter <- function(date_vector) {
  month <- as.double(format(date_vector, "%m"))
  quarter <- ceiling((month-0.1)/3) - 1
  quarter <- ifelse(quarter == 0, 4, quarter)
  paste0(date_col_fy(date_vector), " Q", quarter)

}
