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

  cost_model$cost_dataframe

  # Expect total of 200 forecast and 200 actual spend
  test_data <- cost_model$cost_dataframe %>%
    dplyr::group_by(forecast_actual) %>%
    dplyr::summarise(sum = sum(cost_gbp_real))

  fc <- test_data %>% dplyr::filter(forecast_actual == "forecast") %$% sum
  ac <- test_data %>% dplyr::filter(forecast_actual == "actual") %$% sum
  expect_equal(fc, 200)
  expect_equal(ac, 200)

  # More complex example - test with two sets of complex staff assumptions


})

