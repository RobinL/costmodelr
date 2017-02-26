
test_dates <-


test_dates <- readr::read_csv(system.file("extdata", "test_dates.csv", package="costmodelr"), col_types=readr::cols())
test_dates
library(lubridate)

as.Date(parse_date_time(test_dates$a, "dmy"))
as.Date(parse_date_time(test_dates$date_usa, "dmy"))
as.Date(parse_date_time(test_dates$date_longyear, c("dmy", "dmY")))

as.Date(parse_date_time(test_dates$b, "dmy"))


convert_excel_dates_in_df <- function(df, cols="date") {
  tt <- tryCatch(as.Date(parse_date_time(test_dates$a, c("dmy", "dmY"))), error=function(e) return(TRUE), warning=function(w) return(TRUE))
}

test_dates$a
?parse_date_time


key_dates <- readr::read_csv(system.file("extdata", "key_dates_1.csv", package="costmodelr"), col_types=readr::cols())
uvc <- readr::read_csv(system.file("extdata", "user_variable_costs_1.csv", package="costmodelr"), col_types=readr::cols())
users <- readr::read_csv(system.file("extdata", "users_1.csv", package="costmodelr"), col_types=readr::cols())
uvc

users <- expand_to_time_horizon(users,key_dates)
users <- interpolate_days_numeric(users)
users

al <- as.list(uvc[4,])
str(al)

get_user_variable_costs_chunk(al, users, key_dates)

df <- remove_named_cols_from_df(df, c("quantity_increase_per_user", "quantity_increase_per_user", "total_quantity_increase", "num_users"))
df$price_gbp_real <- al$price_in_original_currency_real * get_xr(al$currency, "GBP") * freq_multiplier[[al$pricefrequency]]




test_agg <- cost_model$cost_dataframe %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(n = n())

all(test_agg$n, 9)
# devtools::document()
# roxygen2::roxygenise()
# covr::package_coverage()
# shine(package_coverage())

list(a=1, b=function(x) {return })


