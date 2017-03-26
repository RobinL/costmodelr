#' Overwrite the default categorisation columns.  These enable the user to break down the results of the cost model into different categories
#' such as 'staff costs' and 'hardware costs' at different levels of granularity.
#'
#' Note that the defaults are 'id', 'category_1', 'cateogory_2', 'category_3'
#' @export
set_categorisation_columns <- function (cost_model, categorisation_columns) {

  cost_model$categorisation_columns <- categorisation_columns
  cost_model

}

#' Add to the current categorisation columns.  These enable the user to break down the results of the cost model into different categories
#' such as 'staff costs' and 'hardware costs' at different levels of granularity.
#'
#' @export
add_to_categorisation_columns <- function (cost_model, categorisation_columns) {

  cost_model$categorisation_columns <- c(cost_model$categorisation_columns, categorisation_columns)
  cost_model

}
