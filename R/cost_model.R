create_cost_model <- function(key_dates) {
  cost_model <- list()
  cost_model$chunks <- list()
  cost_model$id_lookup <- list()
  cost_model$key_dates <- key_dates

  cost_model$id_join_columns <- c(paste0("category_", 1:3), "id")

  #A list of all of the types of assumption (fixed cost, recurring cost, staff utilisation etc.) assumptions which have been registered
  cost_model$registered_modules <- list()
  cost_model

}



run_cost_model <- function(cost_model) {
  for (module in cost_model$registered_modules) {
    cost_model <- module$process_module(cost_model)
  }
  cost_model
}

