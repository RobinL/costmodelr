#' Take an assumptions list, a table of the number of users, and key dates, and create a chunk representing user variable costs
#'
#'
get_user_variable_costs_chunk <- function(assumptions_list, users, key_dates) {
  al <- assumptions_list
  df <- users
  df$id <- al$id

  df$quantity <- df$num_users * al$fixed_initial_quantity_per_user
  df$quantity_increase_per_user <- al$growth_in_quantity_absolute_per_annum_per_user/365.25
  df$total_quantity_increase <- df$quantity_increase_per_user * df$num_users
  df$quantity <- df$quantity + cumsum(df$total_quantity_increase)

  df <- remove_named_cols_from_df(df, c("quantity_increase_per_user", "quantity_increase_per_user", "total_quantity_increase", "num_users"))
  df$price_gbp_real <- al$price_in_original_currency_real * get_xr(al$currency, "GBP") * freq_multiplier[[al$pricefrequency]]

  # Increases in in price
  if (al$growth_in_real_cost_percent_per_annum != 0) {
    df <- apply_percentage_growth_multiplier_to_df_col(df,
                                                       annual_growth=al$growth_in_real_cost_percent_per_annum,
                                                       col_to_increase="price_gbp_real")
  } else {
    df <- apply_absolute_increase_to_df_col(df,
                                            annual_increase=al$growth_in_real_cost_absolute_per_annum,
                                            col_to_increase="price_gbp_real")
  }

  df
}

#' Get a data frame consisting of a single row with the assumption ID and auxiliary information about e.g. the cost category
#'
#'
get_user_variable_costs_id <- function(assumption_list, cost_model) {
  cols_to_keep <- cost_model$id_join_columns
  l <- assumption_list[cols_to_keep]
  tibble::as_data_frame(l)

}

#' Given a cost model, create the chunks corresponding to user variable costs
#'
#' @export
process_user_variable_costs <- function(cost_model) {

  cost_model$registered_modules$user_variable_cost$num_user <- expand_to_time_horizon(cost_model$registered_modules$user_variable_cost$num_user,cost_model$key_dates)
  cost_model$registered_modules$user_variable_cost$num_user <- interpolate_days_numeric(cost_model$registered_modules$user_variable_cost$num_user)

  assumptions_table <- cost_model$registered_modules$user_variable_cost$assumptions
  new_chunks <- list()
  new_ids <- list()

  # Iterate through rows of the assumptions, getting chunks
  for (i in 1:nrow(assumptions_table)){
    this_row <- assumptions_table[i,]
    l <- as.list(this_row)

    chunk <- get_user_variable_costs_chunk(l, cost_model$registered_modules$user_variable_cost$num_user, cost_model$key_dates)
    new_chunks[[l$id]] <- chunk

    id <- get_user_variable_costs_id(l, cost_model)
    new_ids[[l$id]] <- id

  }

  cost_model$chunks <- append(cost_model$chunks, new_chunks)
  cost_model$id_lookup <- append(cost_model$id_lookup, new_ids)

  cost_model
}

#' Add assumptions about recurring costs to the cost model
#'
#' @export
add_user_variable_costs <- function(cost_model, num_users, user_variable_cost_assumptions) {

  cost_model$registered_modules$user_variable_cost <- list()

  cost_model$registered_modules$user_variable_cost$num_users <- num_users

  user_variable_cost_assumptions <- create_id_column(user_variable_cost_assumptions, "uvc_")
  cost_model$registered_modules$user_variable_cost$assumptions <- user_variable_cost_assumptions
  cost_model$registered_modules$user_variable_cost$process_module <- process_user_variable_costs
  cost_model
}
