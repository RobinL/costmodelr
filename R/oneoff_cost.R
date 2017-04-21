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
process_oneoff_costs <- function(cost_model, this_module) {

  id_prefix <- paste0("oo_", this_module$module_number, "_")

  oneoff_cost_assumptions <- this_module$oneoff_cost_assumptions %>%
    convert_excel_dates_in_df() %>%
    create_id_column(id_prefix)

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

  # cost_model$registered_modules should be a list.

  this_module <- list()
  # If this is the first time we've called add_oneoff_costs, then register a new module, otherwise append new data

  this_module$oneoff_cost_assumptions <- oneoff_cost_assumptions
  this_module$module_number <- length(cost_model$registered_modules)

  # Note we don't process at this point, we just say how to proess.
  this_module$process_module <- process_oneoff_costs

  cost_model$registered_modules %<>% lappend(this_module)

  cost_model
}




