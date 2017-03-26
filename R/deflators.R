
get_gdp_deflator_series <- function(base_date=NULL, url="https://www.ons.gov.uk/generator?format=csv&uri=/economy/grossdomesticproductgdp/timeseries/l8gg/qna") {

  lines = readr::read_lines(url)

  skips <- which(stringr::str_detect(lines, "1955 Q1")) - 1

  gdp_deflator_raw <- readr::read_csv(url, skip = skips, col_names = c("date", "gdp_deflator"), col_types = readr::cols())
  gdp_deflator_raw$date <- lubridate::as_date(lubridate::parse_date_time(gdp_deflator_raw$date, orders="yq"))

  # If base_date isn't null, attempt to rebase the series to 100 at the given base_date
  if (!is.null(base_date)) {

    rebase_value <- gdp_deflator_raw %>%
      dplyr::filter(date == base_date) %>%
      dplyr::select(gdp_deflator)

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
  new_quarters <- seq(from = max(gdp_deflator_series$date), to = end_date + months(3), by = "quarter")
  new_quarters <- new_quarters[new_quarters != max(gdp_deflator_series$date)]

  len <- length(new_quarters)

  annual_growth_rate <- future_gdp_deflator + 1
  quarterly_growth_rate <- annual_growth_rate^0.25
  growth <- (quarterly_growth_rate)^(1:len)

  new_gdp <- max(gdp_deflator_series$gdp_deflator)*growth

  new_df <- tibble::data_frame(date =new_quarters, gdp_deflator=new_gdp)

  rbind(gdp_deflator_series, new_df)
}

get_base_date <- function(gdp_deflator_df) {

  cell <- gdp_deflator_df %>%
    dplyr::mutate(close = abs(gdp_deflator-100)) %>%
    dplyr::arrange(close) %>%
    head(n=1)

  cell[[1,1]]

}


add_greenbook_discount <- function(gdp_deflator_df,green_book_discount_rate = 0.035) {

  # also add in green book discount, with base date (where index value is 100), at the same date
  base_date <- get_base_date(gdp_deflator_df)

  gbr <- green_book_discount_rate + 1

  gdp_deflator_df <- gdp_deflator_df %>%
    dplyr::mutate(n = row_number()) %>%
    dplyr::mutate(green_book_discount = (gbr^(1/365.25))^(n))

  rebase <- (gdp_deflator_df %>%
               dplyr::filter(date == base_date) %>%
               dplyr::select(green_book_discount))[[1]]

  gdp_deflator_df <- gdp_deflator_df %>%
    dplyr::mutate(green_book_discount = (green_book_discount/rebase)*100) %>%
    dplyr::select(-n)

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
    dplyr::mutate(gdp_deflator = constant_growth_interpolation(gdp_deflator))

  df_inc_greenbook_discount <- add_greenbook_discount(expanded)

  # If base_date isn't null, attempt to rebase the series to 100 at the given base_date
  if (!is.null(base_date)) {

    rebase_values <- df_inc_greenbook_discount %>%
      dplyr::filter(date == base_date) %>%
      dplyr::select(gdp_deflator, green_book_discount)


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
    dplyr::filter(date >= start_date) %>%
    dplyr::filter(date <= end_date)

  # Finally turn into a divisor
  df_inc_greenbook_discount <- df_inc_greenbook_discount %>%
    dplyr::mutate(green_book_discount = green_book_discount/100) %>%
    dplyr::mutate(gdp_deflator = gdp_deflator/100)

  df_inc_greenbook_discount

}


add_real_nominal_costs_to_cost_model <- function(cost_model) {

  start_date <- min(cost_model$key_dates$date)
  end_date <- max(cost_model$key_dates$date)
  deflators <- get_gdp_deflator_for_all_days(start_date = start_date, end_date = end_date, base_date = cost_model$base_date)



  cost_model$cost_dataframe <- cost_model$cost_dataframe %>%
    dplyr::left_join(deflators, by="date")

  #When real_or_nominal is 'real' then price_gbp is price.  Otherwise we need to apply the GDP deflator
  reals <- cost_model$cost_dataframe %>%
    dplyr::filter(real_or_nominal == "real") %>%
    dplyr::mutate(cost_gbp_real = price_gbp*quantity) %>%
    dplyr::mutate(cost_gbp_nominal = price_gbp*gdp_deflator*quantity)

  nominals <- cost_model$cost_dataframe %>%
    dplyr::filter(real_or_nominal == "nominal") %>%
    dplyr::mutate(cost_gbp_real = (price_gbp/gdp_deflator)*quantity) %>%
    dplyr::mutate(cost_gbp_nominal = price_gbp*quantity)

  cost_model$cost_dataframe <- rbind(reals, nominals)

  cost_model$cost_dataframe <- cost_model$cost_dataframe %>%
                                dplyr::select(-price_gbp, -real_or_nominal)


  cost_model

}


