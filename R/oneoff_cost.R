# Oneoff cost

#' Model oneoff costs from an assumptions row, outputting a chunk
#' Each chunk is a number of rows corresponding to the same input assumption
#' with expected columns given by the vector
get_oneoff_cost_chunk <- function(assumption_row, key_dates) {
  expected_cols <- c( "price_in_original_currency", "currency", "quantity", "date", 'real_or_nominal')
  stop_expected_fields(expected_cols, assumption_row)

  assumption_row$price_gbp <- assumption_row$price_in_original_currency * get_xr(assumption_row$currency, "GBP")

  return_columns <- c("date", "id", "quantity", "price_gbp", "real_or_nominal")
  assumption_row[return_columns]
}

#' Get oneoff costs id
#' For other modules, each input assumption row can cause many output rows
#' We therefore split out information about the categorisation of each id, and the rows themselves, to save memory
#' This function gets all of the relevant categorisation information about an id
get_oneoff_cost_id <- function(assumption_row, cost_model) {
  cols_to_keep <- cost_model$categorisation_columns
  assumption_row[cols_to_keep]
}

#' This function is called when the cost model is run.
process_oneoff_costs <- function(cost_model) {
  oneoff_cost_assumptions <- cost_model$registered_modules$oneoff_cost$oneoff_cost_raw
  oneoff_cost_assumptions <- convert_excel_dates_in_df(oneoff_cost_assumptions)
  oneoff_cost_assumptions <- create_id_column(oneoff_cost_assumptions, "oo_")
  cost_model$registered_modules$oneoff_cost$assumptions <- oneoff_cost_assumptions

  # Each row of assumptions creates a chunk (which in general could have more than one row)
  # This chunk ends up in a column called tibbles (each element in this column is a tibble)
  # Then we bind them together
  new_chunks <- oneoff_cost_assumptions %>%
    purrr::by_row(get_oneoff_cost_chunk, key_dates=cost_model$key_dates, .labels=FALSE, .to = "tibbles") %$%
    dplyr::bind_rows(tibbles) # Note that the tibbles column is a list-column

  # Append all new chunk rows to existing chunks
  cost_model$chunks <- dplyr::bind_rows(new_chunks, cost_model$chunks)

  new_ids <- oneoff_cost_assumptions %>%
    purrr::by_row(get_oneoff_cost_id, cost_model=cost_model, .labels=FALSE, .to = "tibbles") %$%
    dplyr::bind_rows(tibbles)

  cost_model$id_lookup <- dplyr::bind_rows(new_ids, cost_model$id_lookup)

  cost_model
}

#' Add assumptions about oneoff costs to the cost model
#'
#' @export
add_oneoff_costs <- function(cost_model, oneoff_cost_assumptions) {

  # If this is the first time we've called add_oneoff_costs, then register a new module, otherwise append new data
  if (!("oneoff_cost" %in% names(cost_model$registered_modules))) {
    cost_model$registered_modules$oneoff_cost <- list()
    cost_model$registered_modules$oneoff_cost$oneoff_cost_raw <- oneoff_cost_assumptions

    # Note we don't process at this point, we just say how to proess.
    # Appending is therefore just a case of adding more assumption rows.  The only difficulty is the id column.
    cost_model$registered_modules$oneoff_cost$process_module <- process_oneoff_costs
  } else {
    cost_model$registered_modules$oneoff_cost$oneoff_cost_raw %<>%
      dplyr::bind_rows(oneoff_cost_assumptions)
  }

  cost_model
}




