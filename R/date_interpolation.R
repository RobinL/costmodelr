#' Take a dataframe with a date column, and return an ordered list of each day in the date range
#'
#' @export
get_all_dates <- function(df, date_col="date", by="day") {

  if (!class(df)[1] == "tbl_df") {
    stop("Must pass a tibble not a data frame or anything else")
  }

  df[[date_col]] <- as.Date(df[[date_col]])
  zoo_dates <- zoo::zoo(df[[date_col]], df[[date_col]])
  seq(start(zoo_dates), end(zoo_dates), by=by)
}


#' Create a dataframe with a row for each day of the dates in the date col, and NAs for any day not in the original df#'
#'
#' @importFrom magrittr %>%
#' @name %>%#'
#' @export
get_all_dates_df <- function(df, date_col="date", by="day") {

  df[[date_col]] <- as.Date(df[[date_col]])
  if (any(duplicated(df[[date_col]]))) {
    stop("There are duplicated dates in your data frame, remove them before continuing.")
  }

  dates <- get_all_dates(df, date_col = date_col )
  dates_df <- tibble::as_data_frame(dates)
  names(dates_df) <- date_col #Rename the column to use the date_col

  merged_df <- dates_df %>%
    dplyr::left_join(df, by=date_col)

  # merged_df <- merge(dates_df, df, by.x=date_col, by.y=date_col, all.x=TRUE)
  merged_df
}

#' Take a datafrome created by 'get_all_dates_df' and interpolate over the NAs to give daily data
#'
#'
interpolate_days <- function(df, date_col="date", interpolation_fn = zoo::na.approx, col_type=is.numeric) {
  df <- get_all_dates_df(df, date_col=date_col, by="day")

  # zoo assumes all of the values are of the same type - either numeric or factor so we can only include characters
  char_cols <- colnames(df)[sapply(df, col_type)]
  cols <- c(date_col, char_cols)
  df <- df[cols]

  z_series <- zoo::zoo(remove_named_cols_from_df(df, date_col), df[[date_col]])
  df <- tibble::as_data_frame(interpolation_fn(z_series))
  df <- tibble::rownames_to_column(df, date_col)
  df[[date_col]] <- as.Date(df[[date_col]])
  df
}

#' Take a datafrome created by 'get_all_dates_df' and interpolate over the NAs in character columns
#'
#' @export
interpolate_days_character <- function(df, date_col="date", interpolation_fn = zoo::na.locf) {
  # If user has passed dataframe with only factors and no characters, throw error
  char_bool <- sapply(df, is.character)
  fac_bool <- sapply(df, is.factor)

  if( !(all(char_bool)) & any(fac_bool)) {
    stop("You need to pass characters not factors to this function")
  }

  df <- interpolate_days(df, date_col=date_col, interpolation_fn=interpolation_fn, col_type=is.character)
  # Zoo turns characters into factors.  We want to turn them back
  char_cols <- sapply(df, is.factor)
  df[char_cols] <- lapply(df[char_cols], as.character)
  df

}

#' Take a datafrome created by 'get_all_dates_df' and interpolate over the NAs in numeric columns
#'
#' @export
interpolate_days_numeric <- function(df, date_col="date", interpolation_fn = zoo::na.approx) {
  interpolate_days(df, date_col=date_col, interpolation_fn=interpolation_fn, col_type=is.numeric)
}
