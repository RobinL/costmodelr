context("Test time groupings")
test_that("Test time groupings", {

  dates_df <- readr::read_csv(system.file("extdata", "time_groups.csv", package="costmodelr"), col_types=readr::cols())
  dates <- dates_df$dates

  testthat::expect_equal(date_col_week(dates), dates_df$date_col_week)
  testthat::expect_equal(date_col_month(dates), dates_df$date_col_month)
  testthat::expect_equal(as.integer(date_col_year(dates)), dates_df$date_col_year)
  testthat::expect_equal(date_col_quarter(dates), dates_df$date_col_quarter)
  testthat::expect_equal(date_col_fy(dates), dates_df$date_col_fy)
  testthat::expect_equal(date_col_fy_quarter(dates), dates_df$date_col_fy_quarter)

})
