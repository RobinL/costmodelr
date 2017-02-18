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
#' @param key_dates a key dates dataframe so the recurring costs have an end date
#' @export
recurring_cost <- function(assumption_list, key_dates) {

  if (growth_in_real_cost_absolute_per_annum != 0 & growth_in_real_cost_percent_per_annum !=0) {
    stop("Recurring costs must have either absolute or percentage growth, not both")
  }

  al <- assumption_list
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

  d

}

