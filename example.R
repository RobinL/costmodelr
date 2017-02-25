


key_dates <- readr::read_csv(system.file("extdata", "key_dates_1.csv", package="costmodelr"), col_types=readr::cols())
staff_utilisation <- readr::read_csv(system.file("extdata", "staff_utilisation_1.csv", package="costmodelr"), col_types=readr::cols())
rate_card <- readr::read_csv(system.file("extdata", "rate_card_1.csv", package="costmodelr"), col_types=readr::cols())
key_dates
cost_model <- create_cost_model(key_dates)

cost_model <- add_staff_utilisation(cost_model, staff_utilisation, rate_card)

cost_model <- run_cost_model(cost_model)

test_agg <- cost_model$cost_dataframe %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(n = n())

all(test_agg$n, 9)
# devtools::document()
# roxygen2::roxygenise()
# covr::package_coverage()
# shine(package_coverage())

seq(as.Date("2016-02-29"), as.Date("2018-04-01"), "month")


assumptions <- list(id = "rc_1",
  price_in_original_currency_real = 100,
  currency = "EUR",
  frequency = "month",
  first_date = as.Date("2016-02-07"),
  quantity = 1,
  growth_in_real_cost_percent_per_annum = 0,
  growth_in_real_cost_absolute_per_annum = 0,
  growth_in_quantity_percent_per_annum = 0,
  growth_in_quantity_absolute_per_annum = 0
)

all(expected_cols %in% names(assumptions))

key_dates <- tibble::data_frame(date = as.Date(c("2015-12-01", "2016-01-03", "2017-03-01")),
                                other = 1:3)

get_recurring_cost_chunk(assumptions, key_dates)

value = date_to_multiplier_percentage_growth(as.Date("2021-01-01"), as.Date("2017-01-01"), 0.1)
value
value = date_to_multiplier_percentage_growth(as.Date("2017-07-01"), as.Date("2017-01-01"), 0.1)

value
value^2
rep(1,3)

df <- tibble::data_frame(date=as.Date(c("2017-01-01", "2018-01-01", "2019-01-01")), mycol=rep(1,3))
df <- apply_percentage_growth_multiplier_to_df_col(df, 1, col_to_increase="mycol")
df
vals <- df$mycol - c(1,2,4)
for (i in vals) {
  testthat::expect_equal(i, 0, tolerance=0.01)
}
df
