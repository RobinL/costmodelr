


key_dates <- readr::read_csv(system.file("extdata", "key_dates_1.csv", package="costmodelr"), col_types=readr::cols())
staff_utilisation <- readr::read_csv(system.file("extdata", "staff_utilisation_1.csv", package="costmodelr"), col_types=readr::cols())
rate_card <- readr::read_csv(system.file("extdata", "rate_card_1.csv", package="costmodelr"), col_types=readr::cols())

sapply(rate_card, typeof)
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



