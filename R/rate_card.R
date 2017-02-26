
#' If staff utilisation is provided for a time horizon shorter than key dates, fill in
#'
#' Zero out all dates outside the time horizon provided
expand_staff_utilisation_to_time_horizon <- function(staff_utilisation, key_dates, frequency) {
  orig_dates <- staff_utilisation$date

  staff_utilisation <- expand_to_time_horizon(staff_utilisation, key_dates)
  # We want to set staff utilisation for dates outside the provided data to zero
  # This applies to all cols in the staff utilisation chart (each one relates to a staff member) except for the date col
  staff_utilisation[staff_utilisation$date > max(orig_dates), colnames(staff_utilisation) != "date"] <- 0
  staff_utilisation[staff_utilisation$date < min(orig_dates), colnames(staff_utilisation) != "date"] <- 0

  # This dataframe will be filled forwards, so need to make sure we include the day after the last date with zeros

  if (max(staff_utilisation$date) - max(orig_dates) > 1) {
    #Duplicate last row
    staff_utilisation <- staff_utilisation[c(1:nrow(staff_utilisation), nrow(staff_utilisation)),]

    #Set date on penultimate row to the next date
    staff_utilisation[nrow(staff_utilisation)-1, "date"] <- max(orig_dates) + 1
  }

  staff_utilisation
}

staff_u_id_to_rate_card_id <- function(id) {
  id <- stringr::str_replace(id, '(\\.[0-9]{1,3}$)', "")
  id <- stringr::str_replace(id, '(_[0-9]{1,3}$)', "")
  id
}


get_staff_line_item <- function(col, staff_utilisation, rate_card, key_dates) {

  this_staff_line_item <- staff_utilisation[,c("date",col)]
  id <- colnames(this_staff_line_item)[2]
  this_staff_line_item$id <- id
  colnames(this_staff_line_item) <- c("date", "quantity", "id")

  rc_id <- staff_u_id_to_rate_card_id(id)

  l <- as.list(rate_card[rate_card$id == rc_id,])

  if (l$price_frequency == "day") {
    f_mult <- 5/7

  } else {
    f_mult <- freq_multiplier[[l$price_frequency]]
  }

  this_staff_line_item$price_gbp_real <- l$price_gbp_real * f_mult

  # Finally interpolate foward
  chars <- interpolate_days_character(this_staff_line_item, interpolation_fn =  zoo::na.locf)
  nums <- interpolate_days_numeric(this_staff_line_item, interpolation_fn =  zoo::na.locf)

  nums <- remove_named_cols_from_df(nums, "date")
  this_staff_line_item <- dplyr::bind_cols(chars, nums)

  # Finally add growth
  this_staff_line_item <- apply_percentage_growth_multiplier_to_df_col(this_staff_line_item,
                                               annual_growth = l$annual_percentage_increase_real,
                                               start_date = kd_min(key_dates),
                                               col_to_increase = "price_gbp_real")
  this_staff_line_item
}

get_staff_line_item_id <- function(cost_model, col,rate_card) {
  cols_to_keep <- cost_model$id_join_columns
  id <- staff_u_id_to_rate_card_id(col)
  rate_card[rate_card$id==id,cols_to_keep]

}

process_staff_utilisation <- function(cost_model){
  staff_utilisation <-   cost_model$registered_modules$staff_utilisation$staff_utilisation
  rate_card <-   cost_model$registered_modules$staff_utilisation$rate_card
  key_dates <- cost_model$key_dates

  stop_duplicated_dates(staff_utilisation)
  stop_duplicated_dates(key_dates)

  staff_utilisation = expand_staff_utilisation_to_time_horizon(staff_utilisation, key_dates)

  staff_line_items <- colnames(staff_utilisation)[colnames(staff_utilisation) != "date"]

  new_chunks <- list()
  new_ids <- list()

  if (any(rate_card$price_frequency == "day")){
    message("Note: Day rates for staff are assumed to apply 5 days per week")
  }

  for (col in staff_line_items) {
    id <- paste0("su_", col)

    this_li <- get_staff_line_item(col, staff_utilisation, rate_card, key_dates)
    this_li$id <- id
    new_chunks[[id]] <- this_li

    this_id <- get_staff_line_item_id(cost_model, col, rate_card)
    this_id$id <- id
    new_ids[[id]] <- this_id
  }

  cost_model$chunks <- append(cost_model$chunks, new_chunks)
  cost_model$id_lookup <- append(cost_model$id_lookup, new_ids)

  cost_model
}

#' Add assumptions about staff utilistion to the cost model
#'
#' @export
add_staff_utilisation <- function(cost_model, staff_utilisation, rate_card) {
  cost_model$registered_modules$staff_utilisation <- list()

  # Check that currency columns are numeric
  stop_if_nonnumeric(rate_card, c("price_gbp_real","annual_percentage_increase_real"))

  # Check that the date column is of type date
  stop_if_not_date(staff_utilisation)

  staff_utilisation <- convert_excel_dates_in_df(staff_utilisation)
  rate_card <- convert_excel_dates_in_df(rate_card)

  cost_model$registered_modules$staff_utilisation$staff_utilisation <- staff_utilisation
  cost_model$registered_modules$staff_utilisation$rate_card <- rate_card
  cost_model$registered_modules$staff_utilisation$process_module <- process_staff_utilisation
  cost_model
}


