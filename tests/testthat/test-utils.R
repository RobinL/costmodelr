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
