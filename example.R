


key_dates <- readr::read_csv(system.file("extdata", "key_dates_1.csv", package="costmodelr"), col_types=readr::cols())
staff_utilisation <- readr::read_csv(system.file("extdata", "staff_utilisation_1.csv", package="costmodelr"), col_types=readr::cols())
rate_card <- readr::read_csv(system.file("extdata", "rate_card_1.csv", package="costmodelr"), col_types=readr::cols())

oneoff <- readr::read_csv(system.file("extdata", "oneoff_costs_1.csv", package="costmodelr"), col_types=readr::cols())
oneoff
this_row <- oneoff[1,]
l <- as.list(this_row)
l$id <- "id1"
chunk <- get_oneoff_cost_chunk(l, key_dates)
chunk
colnames(chunk)

chunk[["price_gbp_real"]]

cost_model <- add_staff_utilisation(cost_model, staff_utilisation, rate_card)

cost_model <- run_cost_model(cost_model)

test_agg <- cost_model$cost_dataframe %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(n = n())

all(test_agg$n, 9)
# devtools::document()
# roxygen2::roxygenise()
# covr::package_coverage()
# shine(package_coverage())
