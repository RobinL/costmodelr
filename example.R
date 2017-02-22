key_dates <- readr::read_csv("inst/extdata/key_dates_1.csv", col_types=readr::cols())

recurring_cost_assumptions =  readr::read_csv("inst/extdata/recurring_cost_1.csv", col_types=readr::cols())

staff_utilisation <- readr::read_csv("inst/extdata/staff_utilisation_1.csv", col_types=readr::cols())
rate_card <- readr::read_csv("inst/extdata/rate_card_1.csv", col_types=readr::cols())

user_variable_costs <- readr::read_csv("inst/extdata/user_variable_costs_1.csv", col_types=readr::cols())
users <- readr::read_csv("inst/extdata/users_1.csv", col_types=readr::cols())

cost_model <- create_cost_model(key_dates)

cost_model <- add_recurring_cost(cost_model, recurring_cost_assumptions)
cost_model <- add_staff_utilisation(cost_model, staff_utilisation, rate_card)
cost_model <- add_user_variable_costs(cost_model, users, user_variable_costs)

cost_model <- run_cost_model(cost_model)



chunks <- do.call(rbind, cost_model$chunks)
id_lookup <- do.call(rbind, cost_model$id_lookup)

chunk


# devtools::document()
# roxygen2::roxygenise()
# #  covr::package_coverage()
# # shine(package_coverage())
