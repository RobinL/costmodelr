context("utils")

test_that("test remove_named_cols_from_df", {
  df <- as.data.frame(matrix(1:100, ncol=10))
  df_new <- remove_named_cols_from_df(df, paste0("V", 3:5))
  expect_true(all(names(df_new) == c("V1", "V2", paste0("V", 6:10))))
  expect_equal(length(names(df_new)),7)
})

test_that("test posixt_cols_to_date", {
  df <- data.frame(d1=lubridate::now(),d2 = as.Date("2017-01-01"), d3 =as.POSIXct(lubridate::now()), d4=as.POSIXlt("2017-01-01") ,data1=1:5, data2="a")
  df$d4 <- as.POSIXlt(df$d4)
  df2 <-posixt_cols_to_date(df)
  date_cols = paste0("d", 1:4)
  expect_false(all(sapply(df[date_cols],class) == "Date"))
  expect_true(all(sapply(df2[date_cols],class) == "Date"))
  expect_false(all(sapply(df2,class) == "Date"))
})

test_that("read_worksheet_from_file_and_tidy",{
  df <- read_worksheet_from_file_and_tidy("assumptions_test_1.xlsx", "key_dates")
  expect_equal(class(df)[1], "tbl_df")
  expect_equal(class(df$date), "Date")
  expect_equal(class(df$name), "character")
})

test_that("stop_duplicated_dates", {
  df <- tibble::data_frame(a = as.Date(c("2017-01-01", "2017-01-01")))
  expect_error(stop_duplicated_dates(df, "a"))

  df <- tibble::data_frame(a = as.Date(c("2017-01-01", "2018-01-01")))
  expect_error(stop_duplicated_dates(df, "a"),NA)
})

test_that("get_xr", {
  expect_equal(get_xr("GBP"), 1.00)
  expect_equal(get_xr("EUR"), 0.85, tolerance=0.4)  #The exchange rate might change!!
  expect_equal(get_xr("USD"), 0.8, tolerance=0.4) 
  expect_equal(get_xr("JPY"), 0.0072, tolerance=0.01)
})

test_that("date_to_multiplier_percentage_growth", {
    value = date_to_multiplier_percentage_growth(as.Date("2021-01-01"), as.Date("2017-01-01"), 0.1)
    expect_equal(value, 1.1^4)

    value1 = date_to_multiplier_percentage_growth(as.Date("2017-06-30"), as.Date("2017-01-01"), 0.1)
    value2 = date_to_multiplier_percentage_growth(as.Date("2018-01-01"), as.Date("2017-06-30"), 0.1)
    value3 = date_to_multiplier_percentage_growth(as.Date("2018-01-01"), as.Date("2017-01-01"), 0.1)
    expect_equal(value1*value2,value3)
})

test_that("apply_percentage_growth_multiplier_to_df_col", {
  df <- tibble::data_frame(date=as.Date(c("2017-01-01", "2018-01-01", "2019-01-01")), mycol=rep(1,3))              
  df <- apply_percentage_growth_multiplier_to_df_col(df, 1, col_to_increase="mycol")
  vals <- df$mycol - c(1,2,4)
  for (i in vals) {
      expect_equal(i, 0, tolerance=0.01)
  }

})

