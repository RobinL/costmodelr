context("test recurring costs module")

assumptions <- list(
    id = "rc_1", 
    price_in_original_currency_real = "100", 
    currency = "EUR",
    frequency = "month",
    first_date = "2016-02-01",
    quantity = 1,
    growth_in_real_cost_percent_per_annum = 0,
    growth_in_real_cost_absolute_per_annum = 0,
    growth_in_quantity_percent_per_annum = 0,
    growth_in_quantity_absolute_per_annum = 0
)


key_dates <- tibble::data_frame(date = as.Date(c("2015-12-01", "2016-01-03", "2018-03-01")),
                                other = 1:3)
test_that("test recurring costs", {


})

test_that("test recurring costs example 1", {

})
