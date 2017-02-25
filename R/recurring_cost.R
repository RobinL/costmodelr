# Recurring cost

#' Model recurring costs from an assumptions list
#' @param assumptions_list this is a list that should have the following elements
#' * id: The cost (line item) ID.  This can be used later to join on other information such as the category of spend, etc
#' * price_in_original_currency_real: The price of the line item
#' * currency: A string.  `GBP` if Sterling, otherwise `USD`, `EUR` etc.
#' * frequency:  How often the cost occurs.  `day`, `week`, `month`, `year` etc.
#' * first date:  a Date. The first time the cost occurs
#' * growth_in_real_cost_percent_per_annum
#' * growth_in_real_cost_absolute_per_annum
#' * growth_in_quantity_percent_per_annum
#' * growth_in_quantity_absolute_per_annum
#' * quantity
#' @param key_dates a key dates dataframe so the recurring costs have an end date
#' @export
get_recurring_cost_chunk <- function(assumption_list, key_dates) {
  al <- assumption_list

  if (al$growth_in_real_cost_absolute_per_annum != 0 & al$growth_in_real_cost_percent_per_annum !=0) {
    stop("Recurring costs must have either absolute or percentage growth, not both")
  }

  expected_cols <- c("price_in_original_currency_real", "currency", "frequency", "first_date", "quantity", "growth_in_real_cost_percent_per_annum", "growth_in_real_cost_absolute_per_annum", "growth_in_quantity_percent_per_annum", "growth_in_quantity_absolute_per_annum")

  if (!(all(expected_cols %in% names(assumption_list)))) {
      message <- paste(c("You are missing some fields.  Expecting the following: ", expected_cols), sep=", ")
      stop(message)
  }


  rc_dates <- seq(al$first_date, to=kd_max(key_dates), by=al$frequency)
  df <- tibble::data_frame("date" = rc_dates, id=al$id)

  df$price_gbp_real <- al$price_in_original_currency_real * get_xr(al$currency, "GBP")
  df$quantity <- al$quantity

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


  # Increases in in quantity
  if (al$growth_in_real_cost_percent_per_annum != 0) {
    df <- apply_percentage_growth_multiplier_to_df_col(df,
                                                       annual_growth=al$growth_in_quantity_percent_per_annum,
                                                       col_to_increase="quantity")
  } else {
    df <- apply_absolute_increase_to_df_col(df,
                                            annual_increase=al$growth_in_quantity_absolute_per_annum,
                                            col_to_increase="quantity")
  }

  df

}

get_recurring_cost_id <- function(assumption_list, cost_model) {
  cols_to_keep <- cost_model$id_join_columns
  l <- assumption_list[cols_to_keep]
  tibble::as_data_frame(l)

}


process_recurring_costs <- function(cost_model) {

  assumptions_table <- cost_model$registered_modules$recurring_cost$assumptions
  new_chunks <- list()
  new_ids <- list()

  # Iterate through rows of the assumptions, getting chunks
  for (i in 1:nrow(assumptions_table)){
    this_row <- assumptions_table[i,]
    l <- as.list(this_row)

    chunk <- get_recurring_cost_chunk(l, cost_model$key_dates)
    new_chunks[[l$id]] <- chunk

    id <- get_recurring_cost_id(l, cost_model)
    new_ids[[l$id]] <- id

  }

  cost_model$chunks <- append(cost_model$chunks, new_chunks)
  cost_model$id_lookup <- append(cost_model$id_lookup, new_ids)

  cost_model
}

#' Add assumptions about recurring costs to the cost model
#'
#' @export
add_recurring_cost <- function(cost_model, recurring_cost_assumptions) {
  cost_model$registered_modules$recurring_cost <- list()
  recurring_cost_assumptions <- create_id_column(recurring_cost_assumptions, "rc_")
  cost_model$registered_modules$recurring_cost$assumptions <- recurring_cost_assumptions
  cost_model$registered_modules$recurring_cost$process_module <- process_recurring_costs
  cost_model
}




