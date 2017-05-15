#' Take an assumptions list, a table of the number of users, and key dates, and create a chunk representing user variable costs
#'
#'
get_user_variable_costs_chunk <- function(assumptions_list, users, key_dates) {
  al <- assumptions_list
  df <- users
  df$id <- al$id
  df$real_or_nominal <- al$real_or_nominal

  df$quantity <- df$num_users * al$fixed_initial_quantity_per_user
  df$quantity_increase_per_user <- al$growth_in_quantity_absolute_per_annum_per_user/365.25
  df$total_quantity_increase <- df$quantity_increase_per_user * df$num_users
  df$quantity <- df$quantity + cumsum(df$total_quantity_increase)

  df <- remove_named_cols_from_df(df, c("quantity_increase_per_user", "quantity_increase_per_user", "total_quantity_increase", "num_users"))
  df$price_gbp <- al$price_in_original_currency * get_xr(al$currency, "GBP") * freq_multiplier[[al$pricefrequency]]

  # Increases in in price
  if (al$growth_in_cost_percent_per_annum != 0) {
    df <- apply_percentage_growth_multiplier_to_df_col(df,
                                                       annual_growth=al$growth_in_cost_percent_per_annum,
                                                       col_to_increase="price_gbp")
  } else {
    df <- apply_absolute_increase_to_df_col(df,
                                            annual_increase=al$growth_in_cost_absolute_per_annum,
                                            col_to_increase="price_gbp")
  }

  df <- df %>%
    dplyr::filter(quantity != 0)
  df
}

#' Get a data frame consisting of a single row with the assumption ID and auxiliary information about e.g. the cost category
#'
#'
get_user_variable_costs_id <- function(assumption_list, cost_model) {
  cols_to_keep <- cost_model$categorisation_columns
  l <- assumption_list[cols_to_keep]
  tibble::as_data_frame(l)

}

#' Given a cost model, create the chunks corresponding to user variable costs
#'
#' @export
process_user_variable_costs <- function(cost_model, this_module) {

  id_prefix <- paste0("uvc_", this_module$module_number, "_")

  user_variable_cost_assumptions <- this_module$user_variable_cost %>%
    convert_excel_dates_in_df() %>%
    create_id_column(id_prefix)

  num_users <- this_module$num_users %>%
    convert_excel_dates_in_df() %>%
    expand_to_time_horizon(cost_model$key_dates) %>%
    interpolate_days_numeric()

  # Iterate through rows of the assumptions, getting chunks
  new_chunks <- user_variable_cost_assumptions %>%
    purrrlyr::by_row(get_user_variable_costs_chunk, users = num_users, key_dates=cost_model$key_dates, .labels = FALSE, .to = "tibbles") %$%
    dplyr::bind_rows(tibbles)

  # Append all new chunk rows to existing chunks
  cost_model$chunks <- dplyr::bind_rows(new_chunks, cost_model$chunks)

  new_ids <- user_variable_cost_assumptions %>%
    purrrlyr::by_row(get_user_variable_costs_id, cost_model=cost_model, .labels=FALSE, .to = "tibbles") %$%
    dplyr::bind_rows(tibbles)

  cost_model$id_lookup <- dplyr::bind_rows(new_ids, cost_model$id_lookup)

  cost_model
}

#' Add assumptions about recurring costs to the cost model
#'
#' @export
add_user_variable_costs <- function(cost_model, num_users, user_variable_cost_assumptions) {

  # cost_model$registered_modules should be a list.

  this_module <- list()
  # If this is the first time we've called add_oneoff_costs, then register a new module, otherwise append new data

  this_module$user_variable_cost <- user_variable_cost_assumptions
  this_module$num_users <- num_users

  this_module$module_number <- length(cost_model$registered_modules)

  # Note we don't process at this point, we just say how to proess.
  this_module$process_module <- process_user_variable_costs

  cost_model$registered_modules %<>% lappend(this_module)

  cost_model

}
