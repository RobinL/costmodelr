context("Test user variable costs")


test_that("User varible costs example 1", {
  key_dates <- readr::read_csv(system.file("extdata", "key_dates_1.csv", package="costmodelr"), col_types=readr::cols())
  uvc <- readr::read_csv(system.file("extdata", "user_variable_costs_1.csv", package="costmodelr"), col_types=readr::cols())
  users <- readr::read_csv(system.file("extdata", "users_1.csv", package="costmodelr"), col_types=readr::cols())

  users <- expand_to_time_horizon(users,key_dates)
  users <- interpolate_days_numeric(users)

  al <- as.list(uvc[4,])

  chunk <- get_user_variable_costs_chunk(al, users, key_dates)

  expect_true(all.equal(2*1.02^(0:9),chunk$price_gbp_real))

  expect_true(all.equal(c(101, 102, 103, 104, 105, 106, 208, 210, 212, 214), chunk$quantity))

})


