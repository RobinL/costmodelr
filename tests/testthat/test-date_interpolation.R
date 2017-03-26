context("Interpolation functions work")

test_that("get_all_dates", {
  test_df <- tibble::data_frame("date1" = c("2017-01-01", "2017-01-05", "2017-01-03"))
  gad_1 <- get_all_dates(test_df, "date1")
  expect_that(class(gad_1), equals("Date"))
  expect_that(gad_1[1], equals(as.Date("2017-01-01")))
  expect_that(gad_1[length(gad_1)], equals(as.Date("2017-01-05")))
  expect_that(length(gad_1), equals( 5))
  expect_that(all(diff(gad_1) == 1), equals(TRUE))

  test_df <- tibble::data_frame("date" = c("2017-04-01", "2017-05-05", "2017-04-03"), "num" = c(1,100,2))
  gad_2 <- get_all_dates(test_df)
  expect_true(all(diff(gad_2)==1))
  expect_equal(min(gad_2), as.Date("2017-04-01"))
  expect_equal(max(gad_2), as.Date("2017-05-05"))
})

test_that("interpolate_days_character", {

  df <- tibble::data_frame(b = as.Date(c("2017-01-01","2017-01-03","2017-01-05")), c = c("a", "b", "c"))
  df_c <- interpolate_days_character(df, date_col = 'b')
  expect_true(all(df_c$c == c("a", "a", "b", "b", "c")))
  expect_true(all(df_c$b == seq(as.Date("2017-01-01"), as.Date("2017-01-05"), by="day")))


  df <- tibble::data_frame(b = as.Date(c("2017-01-01","2017-01-03","2017-01-05")), c = c("a", NA, "c"))
  df_c <- interpolate_days_character(df, date_col = 'b')
  expect_true(all(df_c$c == c("a", "a", "a", "a", "c")))

  df <- tibble::data_frame(date = as.Date(c("2017-01-01","2017-01-03","2017-01-05")), c = c("a", "b", "c"), d = c("x", "y", "z"))
  df_c <- interpolate_days_character(df)
  expect_true(all(df_c$c == c("a", "a", "b", "b", "c")))
  expect_true(all(df_c$d == c("x", "x", "y", "y", "z")))


  # What happens if you pass a data frame rather than a tibble?  Should throw a error.  Note data.frame rather than tibble::data_frame
  df <- data.frame(date = as.Date(c("2017-01-01","2017-01-03","2017-01-05")), c = c("a", "b", "c"), d = c("x", "y", "z") )
  expect_error(interpolate_days_character(df))

})


test_that("interpolate_days_numeric", {

  test_df <- tibble::data_frame(a = c(1,3,5), b = c("2017-01-01","2017-01-03","2017-01-05"), c=c(5,3,1), d=c("a", "b", "c"))
  ipt <- interpolate_days(test_df, date_col = "b")
  expect_that(all(ipt$a==1:5), equals(TRUE))
  expect_that(all(ipt$c==5:1), equals(TRUE))
  expect_that(nrow(ipt), equals(5))

  test_df <- tibble::data_frame(a = c(1,5,3), b = c("2017-01-01","2017-01-05","2017-01-03"), c=c(5,1,3) )
  ipt <- interpolate_days(test_df, date_col = "b")
  expect_that(all(ipt$a==1:5), equals(TRUE))
  expect_that(all(ipt$c==5:1), equals(TRUE))
  expect_that(nrow(ipt), equals(5))

  test_df <- tibble::data_frame(a = c(1,3,7), date = c("2017-01-01","2017-01-03","2017-01-05"))
  ipt <- interpolate_days(test_df, date_col = "date")
  expect_true(all(ipt$a == c(1,2,3,5,7)))

  test_df <- as.data.frame(list(a = c(1,2,3), date = c("2017-01-01","2017-01-07","2017-01-07")))
  expect_error(interpolate_days(test_df, date_col = "b"))

})


test_that("growth interpolation", {

  a <- constant_growth_interpolation(c(1,rep(NA,8),10))
  g <- na.omit(a/lag(a))
  testthat::expect_true(all.equal(max(g), min(g)))

})




