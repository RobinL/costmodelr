# TODO:  This should be really basic - producing a pivot table and a chart that shows cumulative cost growth
# User can control breadkdown by category.
# Pivot table breaks costs down by alpha, beta live as well (row) and category (col) with grand totals


#' A Shiny app that enables the user to explore the cost model
#'
#' @export
shiny_vis <- function(cost_model) {

  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::verticalLayout(
        shiny::titlePanel("Cost model explorer"),
        vegalite::vegaliteOutput("basic_linechart"),
        shiny::wellPanel(
          shiny::radioButtons("granularity", label = shiny::h5("Choose granularity of  breakdown"),
                       choices = list("category_1" = "category_1", "category_2" = "category_2", "category_3" = "category_3"),
                       selected = "category_1"),
          shiny::dateRangeInput("daterange", "Date range to show in charts:",
                         start = "2017-01-23",
                         end   = "2018-01-23",
                         min = "2017-01-23",
                         max = "2020-01-01")
        )
      )
    ),
    server = function(input, output) {


      granularity <- shiny::reactive({
        input$granularity
      })

      output$basic_linechart <- vegalite::renderVegalite({

          vegalite::vegalite() %>%
            vegalite::add_data(cost_model$cost_dataframe) %>%
            vegalite::encode_x("date", type="temporal") %>%
            vegalite::encode_y("cost", aggregate="sum") %>%
            vegalite::encode_color(granularity(), type="nominal") %>%
            vegalite::mark_line()


      })
    }
  )
}
