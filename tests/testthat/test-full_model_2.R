context("test full example 2")


test_that("test full example 2", {

  key_dates <- readr::read_csv(system.file("extdata", "key_dates_2.csv", package="costmodelr"), col_types=readr::cols())
  oneoff <- readr::read_csv(system.file("extdata", "oneoff_costs_2.csv", package="costmodelr"), col_types=readr::cols())
  recurring <- readr::read_csv(system.file("extdata", "recurring_cost_2.csv", package="costmodelr"), col_types=readr::cols())

  cost_model <- create_cost_model(key_dates) %>%
    setting_append_to_categorisation_columns("additional_breakdown") %>%
    setting_deflator_base_date(as.Date("2017-01-01")) %>%
    add_oneoff_costs(oneoff) %>%
    add_recurring_cost(recurring) %>%
    run_cost_model()

  df <- cost_model$cost_dataframe

  df2 <- df %>%
    dplyr::filter(category_1 == "real")

  testthat::expect_equal(unique(df2$cost_gbp_real) , 100)

  df3 <- df %>%
    dplyr::filter(category_1 == "nominal")

  testthat::expect_equal(unique(df3$cost_gbp_nominal) , 100)

  deflator <- (df %>%
                 dplyr::filter(date == as.Date("2021-01-01")) %>%
                 dplyr::select(gdp_deflator))[[1,1]]

  expect_equal(deflator, 1.02^4)

  gb <- (df %>%
           dplyr::filter(date == as.Date("2021-01-01")) %>%
           dplyr::select(green_book_discount))[[1,1]]

  expect_equal(gb, 1.035^4)

  expect_true("additional_breakdown" %in% colnames(df))
  expect_true("period2" %in% colnames(df))

})

