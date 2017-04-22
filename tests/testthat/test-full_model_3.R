context("test full example 3")

# This example tests the ability to add costs of the same type (oneoff, recurring etc.) multiple times
# from multiple different assumptions files

test_that("test full example 3", {

  key_dates <- readr::read_csv(system.file("extdata", "key_dates_3.csv", package="costmodelr"), col_types=readr::cols())
  oneoff_forecast <- readr::read_csv(system.file("extdata", "oneoff_costs_3a.csv", package="costmodelr"), col_types=readr::cols())
  oneoff_actual <- readr::read_csv(system.file("extdata", "oneoff_costs_3b.csv", package="costmodelr"), col_types=readr::cols())

  cost_model <- create_cost_model(key_dates) %>%
    setting_append_to_categorisation_columns("forecast_actual") %>%
    setting_deflator_base_date(as.Date("2017-01-01")) %>%
    add_oneoff_costs(oneoff_forecast) %>%
    add_oneoff_costs(oneoff_actual) %>%
    run_cost_model()

  # Expect total of 200 forecast and 200 actual spend
  test_data <- cost_model$cost_dataframe %>%
    dplyr::group_by(forecast_actual) %>%
    dplyr::summarise(sum = sum(cost_gbp_real))

  fc <- test_data %>% dplyr::filter(forecast_actual == "forecast") %$% sum
  ac <- test_data %>% dplyr::filter(forecast_actual == "actual") %$% sum
  expect_equal(fc, 200)
  expect_equal(ac, 200)

  # More complex example - test with two sets of complex staff assumptions
  key_dates <- readr::read_csv(system.file("extdata", "key_dates_3.csv", package="costmodelr"), col_types=readr::cols())
  staff_utilisation_actual <- readr::read_csv(system.file("extdata", "staff_utilisation_3a.csv", package="costmodelr"), col_types=readr::cols())
  staff_utilisation_forecast <- readr::read_csv(system.file("extdata", "staff_utilisation_3b.csv", package="costmodelr"), col_types=readr::cols())
  rate_card_actual <- readr::read_csv(system.file("extdata", "rate_card_3a.csv", package="costmodelr"), col_types=readr::cols())
  rate_card_forecast <- readr::read_csv(system.file("extdata", "rate_card_3b.csv", package="costmodelr"), col_types=readr::cols())

  cost_model <- create_cost_model(key_dates) %>%
    setting_append_to_categorisation_columns("forecast_actual") %>%
    setting_deflator_base_date(as.Date("2017-01-01")) %>%
    add_staff_utilisation(staff_utilisation_actual, rate_card_actual) %>%
    add_staff_utilisation(staff_utilisation_forecast, rate_card_forecast) %>%
    run_cost_model()

  # Expect total to be:  300+300+30+15 = 645
  expect_equal(sum(cost_model$cost_dataframe$cost_gbp_real), 645)
})

