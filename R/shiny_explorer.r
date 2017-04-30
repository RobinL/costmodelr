# TODO:  This should be really basic - producing a pivot table and a chart that shows cumulative cost growth
# User can control breadkdown by category.
# Pivot table breaks costs down by alpha, beta live as well (row) and category (col) with grand totals
# Add second radiobuttons, allowing the user to choose the cols and rows of the pivot table summary
# On a second tab, add a rpivotTable to the shiny app

# Select a date from slider and bar chart of breakdown of costs at that date.


#' A Shiny app that enables the user to explore the cost model
#'
#' @export
shiny_vis <- function(cost_model) {

  key_date_cat_cols <- names(cost_model$key_dates)[names(cost_model$key_dates) != "date"]
  cat_choices <- c(cost_model$categorisation_columns, key_date_cat_cols)
  cat_choices <- cat_choices[cat_choices != "id"]


  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::verticalLayout(
        shiny::titlePanel("Cost model summary"),

        shiny::wellPanel(
          shiny::fluidRow(
            shiny::column(3,
              shiny::radioButtons("granularity", label = "Choose granularity of breakdown",
                                  choices = cat_choices,
                                  selected = "category_1")
            ),
            shiny::column(6,
              shiny::dateRangeInput("daterange", "Date range for cumulative costs:",
                           start = kd_min(cost_model$key_dates),
                           end   = Sys.Date(),
                           min = kd_min(cost_model$key_dates),
                           max = kd_max(cost_model$key_dates)
              ),
              shiny::dateInput("total_cost_date", "Display total costs up to this date:",
                           value=Sys.Date(),
                           max=kd_max(cost_model$key_dates),
                           min=kd_min(cost_model$key_dates)
              )
            ),
            shiny::column(3,
              shiny::radioButtons("periodicity", label = "Choose periodicity of table",
                                  choices = list("Weekly" = "week", "Monthly" = "month"),
                                  selected = "Monthly")
            )
          )
        ),
        shiny::tabsetPanel(
          shiny::tabPanel("Main",
            shiny::fluidRow(
              shiny::column(6,
                shiny::fluidRow(shiny::h4("Cumulative costs, split by category, nominal £", align="center")),
                shiny::fluidRow(vegalite::vegaliteOutput("basic_linechart"))
              ),
              shiny::column(6,
                shiny::fluidRow(shiny::htmlOutput("total_cost_chart_title", container=shiny::h4)),
                shiny::fluidRow(vegalite::vegaliteOutput("basic_barchart"))
              )
            ),

            shiny::fluidRow(
              shiny::column(4,
                shiny::fluidRow(shiny::h4("Cross tabulation of costs across all dates, nominal £",align="center")),
                shiny::fluidRow(formattable::formattableOutput("formattedtable_xtab")),
                offset=1
              ),
              shiny::column(4,
                shiny::fluidRow(shiny::h4("Costs through time, nominal £",align="center")),
                shiny::fluidRow(formattable::formattableOutput("formattedtable_throughtime")),
                offset=2
              )
            )

          ),
          shiny::tabPanel("Pivot",
                         rpivotTable::rpivotTableOutput("pivot")
          )
        )
      )
    ),
    server = function(input, output) {

      granularity <- shiny::reactive({
        input$granularity
      })

      periodicity <- shiny::reactive({
        input$periodicity
      })

      cum_costs <- shiny::reactive({
        cost_model <- get_cumulative_costs(cost_model, granularity())
        cost_model$cumcost_dataframe %>%
          dplyr::filter(date >= input$daterange[1]) %>%
          dplyr::filter(date <= input$daterange[2])
      })


      output$total_cost_chart_title <- renderText({
        this_date <- input$total_cost_date
        stringr::str_interp("Total costs up to ${this_date}, nominal £")
      })

      output$basic_linechart <- vegalite::renderVegalite({

        vl <- vegalite::vegalite() %>%
          vegalite::cell_size(width=500) %>%
          vegalite::add_data(cum_costs()) %>%
          vegalite::encode_x("date", type="temporal") %>%
          vegalite::encode_y("csum_nominal_gbp", aggregate="sum") %>%
          vegalite::encode_color(granularity(), type="nominal") %>%
          vegalite::axis_y(title="Cumulative cost (nominal, £)") %>%
          vegalite::axis_x(title="Date") %>%
          vegalite::legend_vl(title="Categorisation") %>%
          vegalite::mark_area()

        vl$x$config$scale$round <- FALSE

        vl

      })

      output$basic_barchart <- vegalite::renderVegalite({

        this_data <- cum_costs() %>%
          dplyr::filter(date == input$total_cost_date)

        vl <- vegalite::vegalite() %>%
          vegalite::cell_size(width=500) %>%
          vegalite::add_data(this_data) %>%
          vegalite::encode_y(granularity(), type="nominal", sort = vegalite::sort_def("csum_nominal_gbp", op="sum", order="descending")) %>%
          vegalite::encode_x("csum_nominal_gbp", aggregate="sum") %>%
          vegalite::axis_x(title = "Cumulative cost (nominal, £)") %>%
          vegalite::mark_bar()

        vl


      })




      output$pivot <- rpivotTable::renderRpivotTable({
        rpivotTable::rpivotTable(data = cost_model$cost_dataframe)
      })

      output$formattedtable_xtab <- formattable::renderFormattable({
        get_formattable(cost_model$cost_dataframe, granularity())
      })

      output$formattedtable_throughtime <- formattable::renderFormattable({
        get_costs_equal_timeperiods_formattable(cost_model$cost_dataframe, periodicity())
      })


    }
  )
}

#' Derives cumulative costs, for plotting
#'
#' @export
get_cumulative_costs <- function(cost_model, groupby_vars) {

  # We want the cumulative cost in each category on each day
  # This is tricky because most costs occur on only a few days
  # But for the chart, we need a record for each cost in each category on each day
  # So we need to do quite a bit of processing to ensure this is the case.

  # Get all dates
  all_days <- kd_all_dates_days(cost_model$key_dates)
  all_days <- tibble::data_frame(date = all_days, crossjoin_col = 1)

  # All categories
  cats <- cost_model$cost_dataframe %>%
    dplyr::group_by_(.dots=groupby_vars)  %>%
    dplyr::summarise(crossjoin_col = 1)

  # A row for each category and each date
  all_combinations <- cats %>% dplyr::left_join(all_days)

  # Grab required cost information from cost dataframe
  costs_to_keep <- cost_model$cost_dataframe %>%
    dplyr::select_("date", "cost_gbp_nominal", .dots = groupby_vars)

  # Make sure there are no dupes (costs with several records on the same day under the same category)
  costs_to_keep <- costs_to_keep %>%
    dplyr::group_by_("date", .dots = groupby_vars) %>%
    dplyr::summarise(cost_gbp_nominal = sum(cost_gbp_nominal))

  # Join to 'cartestian product' dataset
  costs_for_cumulation <- all_combinations %>%
    dplyr::left_join(costs_to_keep) %>%
    dplyr::select(-crossjoin_col)

  # Fill new rows with zeros
  costs_for_cumulation[is.na(costs_for_cumulation)] <- 0

  # Compute cumulative costs
  cost_model$cumcost_dataframe <- costs_for_cumulation %>%
    dplyr::group_by_(.dots=groupby_vars) %>%
    dplyr::mutate(csum_nominal_gbp = cumsum(cost_gbp_nominal)) %>%
    dplyr::arrange(date)

  cost_model
}

