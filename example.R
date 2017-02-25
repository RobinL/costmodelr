


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
