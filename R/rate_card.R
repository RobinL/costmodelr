expand_staff_utilisation_to_time_horizon <- function(staff_utilisation, key_dates) {

  orig_dates <- staff_utilisation$date
  staff_utilisation <- expand_to_time_horizon(staff_utilisation, key_dates)
  staff_utilisation[!(staff_utilisation$date %in% orig_dates),colnames(staff_utilisation) != "date"] <- 0
  staff_utilisation
}

staff_u_id_to_rate_card_id <- function(id) {
  id <- stringr::str_replace(id, '(\\.[0-9]{1,3}$)', "")
  id
}

get_staff_line_item <- function(col, staff_utilisation, rate_card, key_dates) {

  this_staff_line_item <- staff_utilisation[,c("date",col)]
  id <- colnames(this_staff_line_item)[2]
  this_staff_line_item$id <- id
  colnames(this_staff_line_item) <- c("date", "quantity", "id")

  rc_id <- staff_u_id_to_rate_card_id(id)

  l <- as.list(rate_card[rate_card$id == rc_id,])

  freq_multiplier = list("week" =  1/7,
                     "day" = 1,
                     "month" = 12/365.25,
                     "year" = 1/365.25)

  this_staff_line_item$price_gbp_real <- l$price_gbp_real * freq_multiplier[[l$price_frequency]]

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

get_all_staff_line_items <- function(staff_utilisation, rate_card, key_dates) {

  staff_line_items <- colnames(staff_utilisation)[colnames(staff_utilisation) != "date"]

  output_list <- list()
  for (col in staff_line_items) {
    output_list[[col]] <- get_staff_line_item(col, staff_utilisation, rate_card, key_dates)
  }

  do.call(rbind, output_list)
}
