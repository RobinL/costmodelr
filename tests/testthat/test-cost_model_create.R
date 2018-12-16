context("Test cost model factory")
test_that("Test cost model factory", {

  # This is required due to https://stackoverflow.com/a/47467017/1779128
  Sys.setenv(TZ = "Europe/London")

  key_dates <- readr::read_csv(system.file("extdata", "key_dates_exceldates.csv", package="costmodelr"), col_types=readr::cols())
  cm <- costmodelr::create_cost_model(key_dates)

  testthat::expect_equal(cm$key_dates$date, as.Date(c("2017-01-01", "2017-05-01", "2017-10-01", "2018-05-01", "2018-10-01", "2019-01-01")))

})
