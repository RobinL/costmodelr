context("shape compatability")

test_that("get_min_max_dates", {

  key_dates <- tibble::data_frame(date = as.Date(c("2017-01-01","2017-01-03","2017-01-05")),
                                 other = 1:3)

  this_df <- tibble::data_frame(date = as.Date(c("2017-01-02","2017-01-03","2017-01-04")),
                                 other = 1:3)

  l <- get_min_max_dates(this_df, key_dates)
  expect_equal(l$max_key_date , as.Date("2017-01-05"))
  expect_equal(l$min_key_date , as.Date("2017-01-01"))

  expect_equal(l$max_this_df , as.Date("2017-01-04"))
  expect_equal(l$min_this_df , as.Date("2017-01-02"))



})

test_that("expand_to_time_horizon", {
  key_dates <- tibble::data_frame(date = as.Date(c("2017-01-01","2017-01-03","2017-01-05")),
                                  other = 1:3)

  this_df <- tibble::data_frame(date = as.Date(c("2017-01-02","2017-01-03","2017-01-04")),
                                other = 1:3)

  df <- expand_to_time_horizon(this_df, key_dates)

  expect_warning(expand_to_time_horizon(key_dates,this_df))

  expect_true(all(df$other == c(1,1,2,3,3)))
  expect_true(all(df$date == as.Date(c("2017-01-01","2017-01-02", "2017-01-03","2017-01-04", "2017-01-05"))))

  key_dates <- tibble::data_frame(date = as.Date(c("2017-01-01","2017-01-01","2017-01-05")),
                                  other = 1:3)

  expect_error(expand_to_time_horizon(this_df, key_dates))

})

test_that("check_date_compatibility", {
  key_dates <- tibble::data_frame(date = as.Date(c("2017-01-01","2017-01-03","2017-01-05")),
                                  other = 1:3)

  this_df <- tibble::data_frame(date = as.Date(c("2017-01-02","2017-01-03","2017-01-04")),
                                other = 1:3)

  expect_false(check_date_compatibility(key_dates, this_df))
  expect_true(check_date_compatibility(key_dates, key_dates))

})


