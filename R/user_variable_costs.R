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
process_user_variable_costs <- function(cost_model) {

  user_variable_cost_assumptions <- cost_model$registered_modules$user_variable_cost$user_variable_cost_raw %>%
    convert_excel_dates_in_df() %>%
    create_id_column("uvc_")

  num_users <- cost_model$registered_modules$user_variable_cost$num_users %>%
    convert_excel_dates_in_df() %>%
    expand_to_time_horizon(cost_model$key_dates) %>%
    interpolate_days_numeric()

  # Iterate through rows of the assumptions, getting chunks
  new_chunks <- user_variable_cost_assumptions %>%
    purrr::by_row(get_user_variable_costs_chunk, users = num_users, key_dates=cost_model$key_dates, .labels = FALSE, .to = "tibbles") %$%
    dplyr::bind_rows(tibbles)

  # Append all new chunk rows to existing chunks
  cost_model$chunks <- dplyr::bind_rows(new_chunks, cost_model$chunks)

  new_ids <- user_variable_cost_assumptions %>%
    purrr::by_row(get_user_variable_costs_id, cost_model=cost_model, .labels=FALSE, .to = "tibbles") %$%
    dplyr::bind_rows(tibbles)

  cost_model$id_lookup <- dplyr::bind_rows(new_ids, cost_model$id_lookup)




  cost_model
}

#' Add assumptions about recurring costs to the cost model
#'
#' @export
add_user_variable_costs <- function(cost_model, num_users, user_variable_cost_assumptions) {

  # If this is the first time we've called add_oneoff_costs, then register a new module, otherwise append new data
  if (!("user_variable_cost" %in% names(cost_model$registered_modules))) {
    cost_model$registered_modules$user_variable_cost <- list()
    cost_model$registered_modules$user_variable_cost$user_variable_cost_raw <- user_variable_cost_assumptions

    # Note we don't process at this point, we just say how to proess.
    # Appending is therefore just a case of adding more assumption rows.  The only difficulty is the id column.
    cost_model$registered_modules$user_variable_cost$process_module <- process_user_variable_costs
  } else {
    cost_model$registered_modules$user_variable_cost$user_variable_cost_raw %<>%
      dplyr::bind_rows(user_variable_cost_assumptions)
  }

  cost_model$registered_modules$user_variable_cost$num_users <- num_users

  cost_model
}
