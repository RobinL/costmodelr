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

#' Get the current exchange rate
#'
#'@export
get_xr <- memoise::memoise(function(from,to="GBP") {
  if (from == "GBP") {
    return(1.00)
  }
  xr <- quantmod::getQuote(paste0(from, to, "=X"))
  xr <- xr[1,"Last"]
  xr
})

#' Convert a date into a multiplier for costs, where costs are growing or shrinking
#'
#' @param annual_growth annual growth rate, where 0.1 = 10\%, -0.05 = -5\% etc.
#' @export
date_to_multiplier_percentage_growth <- function(this_date, start_date, annual_growth) {
  daily_growth <- (1 + annual_growth)^(1/365.25)
  date_multiplier <- daily_growth^(as.double(this_date - start_date))
  date_multiplier
}


#' Take a df with a date col, and generate a multiplier corresponding to growth or reduction at a constant percent
#'
#' @param annual_growth annual growth rate, where 0.1 = 10\%, -0.05 = -5\% etc.
#' @export
apply_percentage_growth_multiplier_to_df_col <- function(df, annual_growth, start_date, col_to_increase, date_col = "date") {
    if (missing(start_date)) {
      start_date <- min(df[[date_col]])
    }
    df$percentage_multiplier_temp <- sapply(df[[date_col]], date_to_multiplier_percentage_growth, start_date = start_date, annual_growth=annual_growth)
    df[[col_to_increase]] <- df$percentage_multiplier_temp * df[[col_to_increase]]
    df <- remove_named_cols_from_df(df, "percentage_multiplier_temp")
    df
}

#' Convert a date into a multiplier for costs, where costs are growing or shrinking by an absolute amount
#'
#' @param annual_increase absolute annual increase
#' @export
date_to_addition_absolute_increase <- function(this_date, start_date, annual_increase) {
  daily_increase <- annual_increase/365.25
  absolute_addition <- daily_increase*(as.double(this_date - start_date))
}

#' Take a df with a date col, and apply a constant rate of increase to a column
#'
#' @param annual_increase annual increase
#' @export
apply_absolute_increase_to_df_col <- function(df, annual_increase, start_date, col_to_increase, date_col = "date") {
  if (missing(start_date)) {
    start_date <- min(df[[date_col]])
  }
  df$absolute_increase_temp <- sapply(df[[date_col]], date_to_addition_absolute_increase, start_date = start_date, annual_increase=annual_increase)
  df[[col_to_increase]] <- df$absolute_increase_temp + df[[col_to_increase]]
  df <- remove_named_cols_from_df(df, "absolute_increase_temp")
  df
}

#' Take a df with date information, and if there is data information in the format 01/01/2017 (Excel outputs this by default) convert to date and warn user
#'
#' @param cols is either a vector of column names, or NULL. If it's NULL, all columns will be scanned
convert_excel_dates_in_df <- function(df, cols=NULL) {

}


create_id_column <- function(df, prefix) {
  if ("id" %in% colnames(df)) {
    stop("Error in creating unique id column.  There's already a column called id")
  }
  df$id <- paste0(prefix, 1:nrow(df))
  df
}

freq_multiplier = list("week" =  1/7,
                       "day" = 1,
                       "month" = 12/365.25,
                       "year" = 1/365.25)


stop_if_nonnumeric <- function(df, col_names=NULL) {

  if (is.null(col_names)) {
    if (!(all(sapply(df, class) %in% c("numeric","integer","Date")))) {
      stop("Your input data should have been numeric but contained some character columns, check for errornous characters like \u00A3 $ etc")
    }
  } else {
    if (!(all(sapply(df, class)[col_names] %in% c("numeric","integer", "Date")))) {
      stop("Your input data should have been numeric but contained some character columns, check for errornous characters like \u00A3 $ etc")
    }
  }
}

stop_if_not_date <- function(df, col_name="date") {
  if (class(df[[col_name]]) != "Date") {
    stop("You need to make sure that the date column is of class Date.  Use the format yyyy-mm-dd for dates, instead of alternatives like 01/01/2017")
  }
}

stop_expected_fields <- function(fields, this_list) {
  if (!(all(fields %in% names(this_list)))) {
    message <- paste(c("You are missing some fields.  Expecting the following: ", fields), sep=", ")
    stop(message)
  }
}



