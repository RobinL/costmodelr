#' A Shiny app that enables the user to explore the cost model
#'
#' @export
shiny_bubble <- function(cost_model) {

  # TODO: roxygen Show total costs at a particular date


  shinyApp(
  ui = fluidPage(

    shiny::verticalLayout(
      shiny::wellPanel(
        shiny::dateInput("total_cost_date", "Display total costs up to this date:",
                         value=Sys.Date(),
                         max=kd_max(cost_model$key_dates),
                         min=kd_min(cost_model$key_dates)
        )
      ),
      shiny::fluidRow(
        hierarchicalbubble::HBOutput("hb")
      )
    ))
  ,
  server = function(input, output) {

    output$hb <- hierarchicalbubble::renderHB({

      # Summarise by category_1, 2, 3 etc.
      df <- cost_model$cost_dataframe %>%
        dplyr::filter(date <= input$total_cost_date) %>%
        dplyr::group_by(category_1, category_2, category_3) %>%
        dplyr::summarise(value = sum(cost_gbp_nominal))

      df_l <- hierarchicalbubble::wide_to_long_hierarchy(df, paste0("category_", 1:3), "value")

      hierarchicalbubble::hierarchical_bubble(df_l)
    })



  }
  )


}
