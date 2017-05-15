# Recurring cost

#' Model recurring costs from an assumptions list
#'
#' @param assumptions_list some assumptions
#' @param key_dates a key dates dataframe so the recurring costs have an end date
get_recurring_cost_chunk <- function(assumption_list, key_dates) {
  al <- assumption_list

  if (al$growth_in_cost_absolute_per_annum != 0 & al$growth_in_cost_percent_per_annum !=0) {
    stop("Recurring costs must have either absolute or percentage growth, not both")
  }

  expected_cols <- c("price_in_original_currency", "real_or_nominal", "currency", "frequency", "first_date", "quantity", "growth_in_cost_percent_per_annum", "growth_in_cost_absolute_per_annum", "growth_in_quantity_percent_per_annum", "growth_in_quantity_absolute_per_annum")

  if (!(all(expected_cols %in% names(assumption_list)))) {
      message <- paste(c("You are missing some fields.  Expecting the following: ", expected_cols), sep=", ")
      stop(message)
  }

  # If the frequency is below daily, it needs to be converted to daily because recurring costs create a row each time they recur
  if (al$frequency == "hour") {
    al$frequency = "day"
    al$price_in_original_currency = al$price_in_original_currency * 24
  }

  rc_dates <- seq(al$first_date, to=kd_max(key_dates), by=al$frequency)
  df <- tibble::data_frame("date" = rc_dates, id=al$id)

  df$price_gbp <- al$price_in_original_currency * get_xr(al$currency, "GBP")
  df$real_or_nominal <- al$real_or_nominal
  df$quantity <- al$quantity

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


  # Increases in in quantity
  if (al$growth_in_cost_percent_per_annum != 0) {
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
  cols_to_keep <- cost_model$categorisation_columns
  l <- assumption_list[cols_to_keep]
  tibble::as_data_frame(l)

}


process_recurring_costs <- function(cost_model, this_module) {

  id_prefix <- paste0("rc_", this_module$module_number, "_")
  recurring_cost_assumptions <- this_module$recurring_cost_assumptions %>%
                                  create_id_column(id_prefix) %>%
                                  convert_excel_dates_in_df(cols = "first_date")

  # Iterate through rows of the assumptions, getting chunks
  new_chunks <- recurring_cost_assumptions %>%
    purrrlyr::by_row(get_recurring_cost_chunk, key_dates=cost_model$key_dates, .labels = FALSE, .to = "tibbles") %$%
    dplyr::bind_rows(tibbles)



  # Append all new chunk rows to existing chunks
  cost_model$chunks <- dplyr::bind_rows(new_chunks, cost_model$chunks)

  new_ids <- recurring_cost_assumptions %>%
    purrrlyr::by_row(get_recurring_cost_id, cost_model=cost_model, .labels=FALSE, .to = "tibbles") %$%
    dplyr::bind_rows(tibbles)

  cost_model$id_lookup <- dplyr::bind_rows(new_ids, cost_model$id_lookup)

  cost_model
}

#' Add assumptions about recurring costs to the cost model
#'
#' @export
add_recurring_cost <- function(cost_model, recurring_cost_assumptions) {

  this_module <- list()
  # If this is the first time we've called add_oneoff_costs, then register a new module, otherwise append new data

  this_module$recurring_cost_assumptions <- recurring_cost_assumptions
  this_module$module_number <- length(cost_model$registered_modules)

  # Note we don't process at this point, we just say how to proess.
  this_module$process_module <- process_recurring_costs

  cost_model$registered_modules %<>% lappend(this_module)

  cost_model
}
