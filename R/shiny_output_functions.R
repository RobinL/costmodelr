
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
  print(formula)
  table <- reshape2::dcast(cost_dataframe, formula, margins="period", fun.aggregate = sum, value.var = "cost_gbp_nominal")
  print(table)
  formatting_list <- get_formattable_formatting_list(table, selection)

  formattable::formattable(table, formatting_list)
}

get_costs_equal_timeperiods_formattable <- function(cost_dataframe, periodicity) {

  lookup <- list()
  lookup[["week"]] <- "Week %W %Y"
  lookup[["month"]]<- "%b %Y"

  this_format <- lookup[[periodicity]]

  df <- cost_dataframe %>%
    dplyr::select(date, cost_gbp_nominal) %>%
    dplyr::group_by(Date= format(date, format=this_format)) %>%
    dplyr::summarise("Sum of nominal cost" = sum(cost_gbp_nominal)) %>%
    dplyr::arrange(as.Date(paste("01", Date),format=paste("%d", this_format)))

  formattable::formattable(df, list(
    "Sum of nominal cost" = formattable::formatter("span",  x ~ formattable::currency(round(x,-2), symbol="£", digits=0, big.mark=","))
  ))


}



