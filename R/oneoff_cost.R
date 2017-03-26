# Oneoff cost

#' Model oneoff costs from an assumptions list
#'
#' @export
get_oneoff_cost_chunk <- function(assumption_list, key_dates) {
  al <- assumption_list

  expected_cols <- c( "price_in_original_currency_real", "currency", "quantity", "date")
  stop_expected_fields(expected_cols, assumption_list)

  al$price_gbp_real <- al$price_in_original_currency_real * get_xr(al$currency, "GBP")

  return_columns <- c("date", "id", "quantity", "price_gbp_real")
  al <- al[return_columns]
  tibble::as_data_frame(al)

}

get_oneoff_cost_id <- function(assumption_list, cost_model) {
  cols_to_keep <- cost_model$categorisation_columns
  l <- assumption_list[cols_to_keep]
  tibble::as_data_frame(l)

}


process_oneoff_costs <- function(cost_model) {

  assumptions_table <- cost_model$registered_modules$oneoff_cost$assumptions
  new_chunks <- list()
  new_ids <- list()

  # Iterate through rows of the assumptions, getting chunks
  for (i in 1:nrow(assumptions_table)){
    this_row <- assumptions_table[i,]
    l <- as.list(this_row)

    chunk <- get_oneoff_cost_chunk(l, cost_model$key_dates)
    new_chunks[[l$id]] <- chunk

    id <- get_oneoff_cost_id(l, cost_model)
    new_ids[[l$id]] <- id

  }

  cost_model$chunks <- append(cost_model$chunks, new_chunks)
  cost_model$id_lookup <- append(cost_model$id_lookup, new_ids)

  cost_model
}

#' Add assumptions about oneoff costs to the cost model
#'
#' @export
add_oneoff_costs <- function(cost_model, oneoff_cost_assumptions) {
  cost_model$registered_modules$oneoff_cost <- list()

  oneoff_cost_assumptions <- convert_excel_dates_in_df(oneoff_cost_assumptions)
  oneoff_cost_assumptions <- create_id_column(oneoff_cost_assumptions, "oo_")
  cost_model$registered_modules$oneoff_cost$assumptions <- oneoff_cost_assumptions
  cost_model$registered_modules$oneoff_cost$process_module <- process_oneoff_costs
  cost_model
}




