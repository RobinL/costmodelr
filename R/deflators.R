
get_gdp_deflator_series <- function(base_date=NULL, url="https://www.ons.gov.uk/generator?format=csv&uri=/economy/grossdomesticproductgdp/timeseries/l8gg/qna") {

  skips <- which(str_detect(lines, "1955 Q1")) - 1

  gdp_deflator_raw <- readr::read_csv(url, skip = skips, col_names = c("date", "gdp_deflator"), col_types = cols())
  gdp_deflator_raw$date <- lubridate::as_date(lubridate::parse_date_time(gdp_deflator_raw$date, orders="yq"))

  # If base_date isn't null, attempt to rebase the series to 100 at the given base_date
  if (!is.null(base_date)) {

    rebase_value <- gdp_deflator_raw %>%
      filter(date == base_date) %>%
      select(gdp_deflator)

    tryCatch({
      rebase_value <- rebase_value[[1,1]]
      gdp_deflator_raw$gdp_deflator <- gdp_deflator_raw$gdp_deflator / rebase_value
    },
    error = function(e) {stop("Please provide a valid base date.  Must be of type date and match one of the dates in the GDP deflator series.")}
    )

  }

  gdp_deflator_raw

}



add_future_values_to_gdp_deflator <- function(gdp_deflator_series, end_date = as.Date("2020-01-01"),
                                              future_gdp_deflator = 0.02) {

  # Add additional rows for future years
  new_quarters <- seq(from = max(gdp_deflator_series$date), to = end_date, by = "quarter")
  new_quarters <- new_quarters[new_quarters != max(gdp_deflator_series$date)]

  len <- length(new_quarters)

  annual_growth_rate <- future_gdp_deflator + 1
  quarterly_growth_rate <- annual_growth_rate^0.25
  growth <- (quarterly_growth_rate)^(1:len)

  new_gdp <- max(gdp_deflator_series$gdp_deflator)*growth

  new_df <- data_frame(date =new_quarters, gdp_deflator=new_gdp)

  rbind(gdp_deflator_series, new_df)
}

get_base_date <- function(gdp_deflator_df) {

  cell <- gdp_deflator_df %>%
    mutate(close = abs(gdp_deflator-100)) %>%
    arrange(close) %>%
    head(n=1)

  cell[[1,1]]

}


add_greenbook_discount <- function(gdp_deflator_df,green_book_discount_rate = 0.035) {

  # also add in green book discount, with base date (where index value is 100), at the same date
  base_date <- get_base_date(gdp_deflator_df)

  gbr <- green_book_discount_rate + 1

  gdp_deflator_df <- gdp_deflator_df %>%
    mutate(n = row_number()) %>%
    mutate(green_book_discount = (gbr^(1/356.25))^(n))

  rebase <- (gdp_deflator_df %>%
               filter(date == base_date) %>%
               select(green_book_discount))[[1]]

  gdp_deflator_df <- gdp_deflator_df %>%
    mutate(green_book_discount = (green_book_discount/rebase)*100) %>%
    select(-n)

  gdp_deflator_df

}

get_gdp_deflator_for_all_days <- function(start_date = as.Date("2017-01-01"), end_date = as.Date("2020-01-01"),
                                          base_date=NULL, future_gdp_deflator = 0.02, green_book_discount_rate = 0.035) {

  gdp_deflator_series <- get_gdp_deflator_series()

  range_d <- range(gdp_deflator_series$date)
  names(range_d) <- c("min", "max")

  if (range_d["max"] < end_date ) {
    gdp_deflator_series <- add_future_values_to_gdp_deflator(gdp_deflator_series, end_date = end_date, future_gdp_deflator = 0.02)
  }

  expanded <- get_all_dates_df(gdp_deflator_series)

  expanded <- expanded %>%
    mutate(gdp_deflator = constant_growth_interpolation(gdp_deflator))

  df_inc_greenbook_discount <- add_greenbook_discount(expanded)

  # If base_date isn't null, attempt to rebase the series to 100 at the given base_date
  if (!is.null(base_date)) {

    rebase_values <- df_inc_greenbook_discount %>%
      filter(date == base_date) %>%
      select(gdp_deflator, green_book_discount)


    tryCatch({
      rebase_value_gdp <- rebase_values$gdp_deflator[[1]]
      rebase_value_gb <- rebase_values$green_book_discount[[1]]

      df_inc_greenbook_discount$gdp_deflator <- (df_inc_greenbook_discount$gdp_deflator / rebase_value_gdp) * 100
      df_inc_greenbook_discount$green_book_discount <- (df_inc_greenbook_discount$green_book_discount / rebase_value_gb) * 100

    },
    error = function(e) {stop("Please provide a valid base date.  Must be of type date and match one of the dates in the GDP deflator series.")}
    )
  }

  df_inc_greenbook_discount <- df_inc_greenbook_discount %>%
    filter(date >= start_date) %>%
    filter(date <= end_date)

  # Finally turn into a divisor
  df_inc_greenbook_discount <- df_inc_greenbook_discount %>%
    mutate(green_book_discount = green_book_discount/100) %>%
    mutate(gdp_deflator = gdp_deflator/100)

  df_inc_greenbook_discount

}


get_gdp_deflator_for_all_days(start_date = as.Date("2015-05-01"), end_date = as.Date("2015-05-03"), base_date = as.Date("2015-05-02"))

