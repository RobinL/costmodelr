context("Test oneoff assumptions")

oneoff <- readr::read_csv(system.file("extdata", "oneoff_costs_1.csv", package="costmodelr"), col_types=readr::cols())
key_dates <- readr::read_csv(system.file("extdata", "key_dates_1.csv", package="costmodelr"), col_types=readr::cols())

oneoff <- convert_excel_dates_in_df(oneoff)
key_dates <- convert_excel_dates_in_df(key_dates)


test_that("One off costs example 1", {

  new_chunks <- list()
  new_ids <- list()

  this_row <- oneoff[1,]
  l <- as.list(this_row)
  l$id <- "id1"

  chunk <- get_oneoff_cost_chunk(l, key_dates)

  cols1 <- c("date", "id", "quantity", "price_gbp", "real_or_nominal")
  cols2 <- colnames(chunk)

  expect_true(identical(sort(cols1), sort(cols2)))
  expect_equal(chunk[["price_gbp"]], 50)
  expect_equal(chunk[["quantity"]], 4)
  expect_equal(chunk[["date"]], as.Date("2017-01-02"))

})

