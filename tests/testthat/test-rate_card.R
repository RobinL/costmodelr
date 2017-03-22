context("rate_card")


staff_utilisation <- tibble::data_frame(date = as.Date(c("2017-02-01", "2017-02-08", "2017-02-15")),
                                        TA.1 = c(0.5,1,0.5),
                                        TA.2 = c(0.5,1,0.5),
                                        PM = c(0.5,1,0.5))


rate_card <- tibble::data_frame(id = c("TA", "PM"),
                                price_gbp_real = c(140, 70),
                                price_frequency = c("week", "week"),
                                annual_percentage_increase_real = c(0.0,0.1)
)


key_dates <- tibble::data_frame(date = as.Date(c("2017-01-01", "2017-01-03", "2018-03-01")),
                                other = 1:3)

test_that("expand_staff_utilisation_to_time_horizon", {
  staff_utilisation <- expand_staff_utilisation_to_time_horizon(staff_utilisation, key_dates)
  dates <- as.Date(c("2017-01-01","2017-02-01", "2017-02-08", "2017-02-15","2017-02-16","2018-03-01"))
  expect_true(all(staff_utilisation$date == dates))
  expect_true(all(staff_utilisation$TA.1 == c(0,0.5,1,0.5,0,0)))
})

test_that("get_staff_line_item", {
  su_expanded <- expand_staff_utilisation_to_time_horizon(staff_utilisation, key_dates)
  sli <- get_staff_line_item("TA.1", su_expanded, rate_card, key_dates)
  expect_equal(sli[[1,"price_gbp_real"]], 20.0)
  expect_equal(sli[[1,"quantity"]], 0)
  expect_true(sli[sli$date == as.Date("2017-02-08"),"quantity"] == 1)
  expect_true(sli[sli$date == as.Date("2017-02-01"),"quantity"] == 0.5)
  expect_true(sli[sli$date == as.Date("2017-02-07"),"quantity"] == 0.5)
  expect_true(sli[sli$date == as.Date("2017-01-31"),"quantity"] == 0)
  expect_true(all(sli$id == "TA.1"))

  su_expanded <- expand_staff_utilisation_to_time_horizon(staff_utilisation, key_dates)
  sli <- get_staff_line_item("PM", su_expanded, rate_card, key_dates)
  expect_equal(sli[sli$date == as.Date("2018-01-01"),"price_gbp_real"][[1]], 11, tolerance=0.1, scale=1)
})

rate_card <- tibble::data_frame(id = c("TA", "PM"),
                                price_gbp_real = c(10, 50),
                                price_frequency = c("working_day", "week"),
                                annual_percentage_increase_real = c(0.0,0))

staff_utilisation <- tibble::data_frame(date = as.Date(c("2017-01-01")),
                                        TA = c(1),
                                        PM = c(1))

key_dates <- tibble::data_frame(date = as.Date(c("2017-01-01", "2017-01-07")),
                                other = 1:2)

test_that("day rates adjust for work week", {
  su_expanded <- expand_staff_utilisation_to_time_horizon(staff_utilisation, key_dates)
  sli1 <- get_staff_line_item("TA", su_expanded, rate_card, key_dates)
  sli2 <- get_staff_line_item("PM", su_expanded, rate_card, key_dates)
  expect_true(all.equal(sli1$price_gbp_real,  sli2$price_gbp_real))

})

# Now run tests against the example data sets
test_that("example 1", {
    key_dates <- readr::read_csv(system.file("extdata", "key_dates_1.csv", package="costmodelr"), col_types=readr::cols())

    expect_warning(staff_utilisation <- readr::read_csv(system.file("extdata", "staff_utilisation_1.csv", package="costmodelr"), col_types=readr::cols()))

    rate_card <- readr::read_csv(system.file("extdata", "rate_card_1.csv", package="costmodelr"), col_types=readr::cols())

    cost_model <- create_cost_model(key_dates)

    cost_model <- add_staff_utilisation(cost_model, staff_utilisation, rate_card)

    cost_model <- run_cost_model(cost_model)

    cost_model$cost_dataframe

    test_agg <- cost_model$cost_dataframe %>%
      dplyr::select(id) %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(n = n())

    expect_true(all(test_agg$n == 10))


})
