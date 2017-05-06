
# Formattable
get_formattable_formula <- function(selection) {

  if (selection == "period") {
    selection <- "category_1"
  }

  selection <- switch(selection,
                        "category_1" = "category_1",
                        "category_2" = "category_1 + category_2",
                        "category_3" = "category_1 + category_2 + category_3"
  )


  return(stringr::str_interp("${selection} ~ period"))

}


get_formattable_formatting_list <- function(table, selection) {
  format_list <- list()

  if (selection == "period") {selection <- "category_1"}

  selection <- switch(selection,
                      "category_1" = "category_1",
                      "category_2" = c("category_1", "category_2"),
                      "category_3" = c("category_1", "category_2", "category_3")
  )

  for (sel in selection) {
    format_list[[sel]] <- formattable::formatter("span", style = x ~ formattable::style(font.weight = "bold"))
  }


  # Get numeric columns, and for each add a formatter than makes it currency
  numerics <- sapply(table, is.numeric)
  numeric_columns <- colnames(table)[numerics]

  for (col in numeric_columns) {
    format_list[[col]] <- formattable::formatter("span",  x ~ formattable::currency(round(x,-2), symbol="£", digits=0, big.mark=","))
  }

  format_list
}



get_formattable <- function(cost_dataframe, selection) {
  formula <- get_formattable_formula(selection)

  margin <- selection
  if (stringr::str_detect(selection, "category_\\d")) {
    margin <- "category_1"
  }

  table <- reshape2::dcast(cost_dataframe, formula, margins=c(margin, "period"), fun.aggregate = sum, value.var = "cost_gbp_nominal")
  print(table)
  formatting_list <- get_formattable_formatting_list(table, selection)

  formattable::formattable(table, formatting_list)
}

get_costs_equal_timeperiods_formattable <- function(cost_dataframe, periodicity) {

  lookup <- list()
  lookup[["date_col_week"]] <- date_col_week
  lookup[["date_col_month"]] <- date_col_month
  lookup[["date_col_year"]] <- date_col_year
  lookup[["date_col_quarter"]] <- date_col_quarter
  lookup[["date_col_fy"]] <- date_col_fy
  lookup[["date_col_fy_quarter"]] <- date_col_fy_quarter

  this_format_function <- lookup[[periodicity]]

  df <- cost_dataframe %>%
    dplyr::select(date, cost_gbp_nominal) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(date_formatted = this_format_function(date)) %>%
    dplyr::group_by(Date= date_formatted) %>%
    dplyr::summarise("Sum of nominal cost" = sum(cost_gbp_nominal))

  formattable::formattable(df, list(
    "Sum of nominal cost" = formattable::formatter("span",  x ~ formattable::currency(round(x,-2), symbol="£", digits=0, big.mark=","))
  ))


}



