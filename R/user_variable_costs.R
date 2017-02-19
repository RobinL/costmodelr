uvc =  readr::read_csv("inst/extdata/user_variable_costs_1.csv", col_types=readr::cols())

uvc <- create_id_column(uvc, "uvc_")

users =  readr::read_csv("inst/extdata/users_1.csv", col_types=readr::cols())
key_dates =  readr::read_csv("inst/extdata/key_dates_1.csv", col_types=readr::cols())

al <- as.list(uvc[1,])
str(al)
# Need to convert all costs to daily
users <- expand_to_time_horizon(users,key_dates)
users <- interpolate_days_numeric(users)

# Need to get daily price

users$quantity <- 0
df <- users
df$id <- al$id

df$quantity_increase_per_user <- al$growth_in_quantity_absolute_per_annum_per_user/365.25
df$total_quantity_increase <- df$quantity_increase_per_user * df$num_users

df$quantity <- cumsum(df$total_quantity_increase)

df <- remove_named_cols_from_df(df, c("quantity_increase_per_user", "quantity_increase_per_user", "total_quantity_increase", "num_users"))
df$price_gbp_real <- al$price_in_original_currency_real * get_xr(al$currency, "GBP") * freq_multiplier[[al$pricefrequency]]
df
