#' Get the minimum and maximum dates from two dataframes for later compairson
#'
#'
get_min_max_dates <- function(this_df, key_dates, this_df_date_col="date", key_dates_date_col="date") {
  return_list <- list()

  return_list$max_key_date <- max(key_dates[[key_dates_date_col]])
  return_list$min_key_date <- min(key_dates[[key_dates_date_col]])

  return_list$max_this_df <- max(this_df[[this_df_date_col]])
  return_list$min_this_df <- min(this_df[[this_df_date_col]])

  return_list
}

#' Check that two dataframes span the same range of dates
#'
#' @export
check_date_compatibility <- function(this_df, key_dates, this_df_date_col="date", key_dates_date_col="date") {
  # Get min max dates from key_dates
  l <- get_min_max_dates(this_df, key_dates, this_df_date_col= this_df_date_col, key_dates_date_col = key_dates_date_col)
  return(l$max_key_date == l$max_this_df & l$min_key_date == l$min_this_df)
}

#' Given a dataframe, expand the time horizon out to the minimum and max
#'
#' Will throw a warning if this df contains dates outside of the key dates range
#' @export
expand_to_time_horizon <- function(this_df, key_dates, this_df_date_col="date", key_dates_date_col="date") {

  stop_duplicated_dates(this_df, date_col = this_df_date_col)
  stop_duplicated_dates(key_dates, date_col=key_dates_date_col)

  l <- get_min_max_dates(this_df, key_dates, this_df_date_col= this_df_date_col, key_dates_date_col = key_dates_date_col)


  if (l$min_this_df < l$min_key_date) {
    warning("Your table of assumptions contains dates outside the key dates range")
  }


  if (l$max_this_df >  l$max_key_date) {
    warning("Your table of assumptions contains dates outside the key dates range")
  }

  if (l$min_this_df > l$min_key_date) {
    # Need to interpolate backwards

    # Get the row to duplicate backwards
    min_row <- this_df[this_df[[this_df_date_col]] == l$min_this_df,]
    min_row[1,this_df_date_col] <- l$min_key_date
    this_df <- rbind(min_row, this_df)

  }


  if (l$max_this_df <  l$max_key_date) {
    # Interpolate fowards to max key date
    max_row <- this_df[this_df[[this_df_date_col]] == l$max_this_df,]
    max_row[1,this_df_date_col] = l$max_key_date
    this_df <- rbind(this_df,max_row)

  }

  this_df


}
