key_dates <- readr::read_csv("inst/extdata/key_dates_1.csv", col_types=readr::cols())
recurring_cost_assumptions =  readr::read_csv("inst/extdata/recurring_cost_1.csv", col_types=readr::cols())

cost_model <- create_cost_model(key_dates)
cost_model <- add_recurring_cost(cost_model, recurring_cost_assumptions)

cost_model <- add_staff_utilisation()

cost_model <- add_user_variable_costs(cost_model, num_users, user_variable_cost_assumptions)


cost_model <- run_cost_model(cost_model)

do.call(rbind, cost_model$chunks)
do.call(rbind, cost_model$id_lookup)
