#' Removed named columns from a dataframe
#'
#' @export
remove_named_cols_from_df <- function(df, drops) {
  df[,!(names(df) %in% drops)]
}

#' Convert all datatime (posixt) columns to be dates.
#'
#' This is important because data joins on the datetime column can go wrong as a result of things like BST.
posixt_cols_to_date <- function(df) {
  cols <- sapply(df, lubridate::is.POSIXt)
  df[cols] <- lapply(df[cols], as.Date)
  df
}

#' Read a worksheet from an Excel file and convert to a tibble.
#'
#' This function also ensures that all relevant columns contains dates rather than datetimes.
#' @export
read_worksheet_from_file_and_tidy <- function(path, sheetname) {
  df <- XLConnect::readWorksheetFromFile(path, sheetname)
  df <- posixt_cols_to_date(df)
  tibble::as_tibble(df)
}

#' Check that there aren't any duplicated dates
#'
#' @export
stop_duplicated_dates <- function(df, date_col = 'date') {
  if (any(duplicated(df[[date_col]]))) {
    stop("There seem to be duplicated rows in your dataframe (i.e. multiple rows for a single date")
  }

}
