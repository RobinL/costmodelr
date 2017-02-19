context("rate_card")

# staff_utilisation <- tibble::data_frame(date = as.Date(c("2017-02-01", "2017-02-08", "2017-02-15")),
#                                         TA.1 = c(0.5,1,0.5),
#                                         TA.2 = c(0.5,1,0.5),
#                                         PM = c(0.5,1,0.5))
# write.csv(staff_utilisation, "inst/extdata/staff_utilisation_1.csv", row.names=FALSE)
# readr::read_csv("inst/extdata/staff_utilisation_1.csv")

staff_utilisation <- readr::read_csv(system.file("extdata", "staff_utilisation_1.csv", package="costmodelr"), col_types=readr::cols())

rate_card <- tibble::data_frame(id = c("TA", "PM"),
                                price_gbp_real = c(140, 70),
                                price_frequency = c("week", "week"),
                                annual_percentage_increase_real = c(0.0,0.1)
)




test_that("staff_u_id_to_rate_card_id", {
  expect_equal(staff_u_id_to_rate_card_id("TA.1"), "TA")
  expect_equal(staff_u_id_to_rate_card_id("TA.12"), "TA")
  expect_equal(staff_u_id_to_rate_card_id("TA1"), "TA1")
  expect_equal(staff_u_id_to_rate_card_id("TA12"), "TA12")
})

test_that("expand_staff_utilisation_to_time_horizon", {
  staff_utilisation <- expand_staff_utilisation_to_time_horizon(staff_utilisation, key_dates)
  dates <- as.Date(c("2017-01-01","2017-02-01", "2017-02-08", "2017-02-15","2018-03-01"))
  expect_true(all(staff_utilisation$date == dates))
  expect_true(all(staff_utilisation$TA.1 == c(0,0.5,1,0.5,0)))
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
  expect_equal(sli[sli$date == as.Date("2018-01-01"),"price_gbp_real"][[1]], 11, tolerance=0.1)
})

test_that("get_all_staff_line_items", {
  all <- get_all_staff_line_items(staff_utilisation, rate_card, key_dates)

  # Same number of rows of each type
  agg <- all %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(count = n())

  expect_true(all(agg$count==425))

})



