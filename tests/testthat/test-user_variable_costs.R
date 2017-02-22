# uvc =  readr::read_csv("inst/extdata/user_variable_costs_1.csv", col_types=readr::cols())
# uvc <- create_id_column(uvc, "uvc_")
# uvc
#
# users =  readr::read_csv("inst/extdata/users_1.csv", col_types=readr::cols())
# users
#
# key_dates =  readr::read_csv("inst/extdata/key_dates_1.csv", col_types=readr::cols())
#
# al <- as.list(uvc[1,])
# str(al)

# Need to convert all costs to daily
# users <- expand_to_time_horizon(users,key_dates)
# users <- interpolate_days_numeric(users)

# Need to get daily price
