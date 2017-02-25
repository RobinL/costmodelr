#
#
#
# key_dates <- readr::read_csv(system.file("extdata", "key_dates_1.csv", package="costmodelr"), col_types=readr::cols())
# staff_utilisation <- readr::read_csv(system.file("extdata", "staff_utilisation_1.csv", package="costmodelr"), col_types=readr::cols())
# rate_card <- readr::read_csv(system.file("extdata", "rate_card_1.csv", package="costmodelr"), col_types=readr::cols())
#
# sapply(rate_card, typeof)
# cost_model <- add_staff_utilisation(cost_model, staff_utilisation, rate_card)
#
# cost_model <- run_cost_model(cost_model)
#
# test_agg <- cost_model$cost_dataframe %>%
#   dplyr::group_by(id) %>%
#   dplyr::summarise(n = n())
#
# all(test_agg$n, 9)
# # devtools::document()
# # roxygen2::roxygenise()
# # covr::package_coverage()
# # shine(package_coverage())
#
# seq(as.Date("2016-02-29"), as.Date("2018-04-01"), "month")
#
#
# assumptions <- list(id = "rc_1",
#   price_in_original_currency_real = 100,
#   currency = "EUR",
#   frequency = "month",
#   first_date = as.Date("2016-02-07"),
#   quantity = 1,
#   growth_in_real_cost_percent_per_annum = 0,
#   growth_in_real_cost_absolute_per_annum = 0,
#   growth_in_quantity_percent_per_annum = 0,
#   growth_in_quantity_absolute_per_annum = 0
# )
#
#
rate_card <- tibble::data_frame(id = c("TA", "PM"),
                                price_gbp_real = c(10, 50),
                                price_frequency = c("day", "week"),
                                annual_percentage_increase_real = c(0.0,0))

staff_utilisation <- tibble::data_frame(date = as.Date(c("2017-01-01")),
                                        TA = c(1),
                                        PM = c(1))
#
key_dates <- tibble::data_frame(date = as.Date(c("2017-01-01", "2017-01-08")),
                                other = 1:2)

key_dates
expand_to_time_horizon(staff_utilisation, key_dates)

su_expanded <- expand_staff_utilisation_to_time_horizon(staff_utilisation, key_dates)
su_expanded

sli1 <- get_staff_line_item("TA", su_expanded, rate_card, key_dates)
sli2 <- get_staff_line_item("PM", su_expanded, rate_card, key_dates)

sli1
sli2
all.equal(sli1$price_gbp_real , sli2$price_gbp_real)
