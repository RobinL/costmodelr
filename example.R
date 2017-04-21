# install.packages("devtools")
# install.packages("dplyr")
# install.pacakges("lubridate")
# install.packages("devtools")
# install.package("readr)
# library(devtools)
# install_github("RobinL/costmodelr")

# detach("package:costmodelr", unload = TRUE)
# library(costmodelr)
library(dplyr)
library(lubridate)
library(readr)
library(magrittr)
library(formattable)
library(threelittlecircles)


# Read in assumptions from files

key_dates <- readr::read_csv("/Users/robinlinacre/Documents/r_projects/final_cost_model/assumptions/key_dates.csv", col_types=readr::cols())
recurring_costs =  readr::read_csv( "/Users/robinlinacre/Documents/r_projects/final_cost_model/assumptions/recurring_cost.csv", col_types=readr::cols())
recurring_costs
staff_utilisation <- readr::read_csv( "/Users/robinlinacre/Documents/r_projects/final_cost_model/assumptions/staff_utilisation_actuals.csv", col_types=readr::cols())

rate_card <- readr::read_csv( "/Users/robinlinacre/Documents/r_projects/final_cost_model/assumptions/rate_card.csv", col_types=readr::cols())

user_variable_costs <- readr::read_csv( "/Users/robinlinacre/Documents/r_projects/final_cost_model/assumptions/user_variable_costs.csv", col_types=readr::cols())
users <- readr::read_csv( "/Users/robinlinacre/Documents/r_projects/final_cost_model/assumptions/users.csv", col_types=readr::cols())

oneoff <- readr::read_csv( "/Users/robinlinacre/Documents/r_projects/final_cost_model/assumptions/oneoff_costs.csv", col_types=readr::cols())


# Add each set of assumptions to model
cost_model <- create_cost_model(key_dates) %>%
  add_oneoff_costs(oneoff) %>%
  add_oneoff_costs(oneoff) %>%
  add_recurring_cost(recurring_costs) %>%
  add_recurring_cost(recurring_costs) %>%
  add_user_variable_costs(users, user_variable_costs) %>%
  add_user_variable_costs(users, user_variable_costs)
  # add_staff_utilisation(staff_utilisation, rate_card)

# Run model
cost_model <- run_cost_model(cost_model)

cost_model$cost_dataframe %>%
  arrange(category_3, date)


shiny_vis(cost_model)

cost_model$id_lookup

cost_model$chunks


cost_model <- get_cumulative_costs(cost_model, "category_1")

vegalite::vegalite() %>%
  vegalite::cell_size(width=500) %>%
  vegalite::add_data(cost_model$cumcost_dataframe) %>%
  vegalite::encode_y("category_1", type="nominal", sort = vegalite::sort_def("csum_nominal_gbp", op="sum", order="descending")) %>%
  vegalite::encode_x("csum_nominal_gbp", aggregate="sum") %>%
  vegalite::
  vegalite::mark_bar()

reshape2::dcast(cost_model$cost_dataframe, period ~ category_1, margins=TRUE, fun.aggregate = sum, value.var = "cost_gbp_nominal")

format(as.Date("2017-01-01"), format="%w %m/%d/%y")

df <- cost_model$cost_dataframe

lookup <- list()
lookup[["week"]] <- "%Y Week %W"
lookup[["month"]]<- "%Y %b"

df %>%
  dplyr::select(date, cost_gbp_nominal) %>%
  dplyr::group_by("Date" = format(date, format="%b %Y")) %>%
  dplyr::summarise("Sum of nominal cost" = sum(cost_gbp_nominal)) %>%
  dplyr::arrange(as.Date(paste("01 ", Date),format="%d %b %Y")) -> df2

formattable::formattable(df2, list(
  "Sum of nominal cost" = formattable::formatter("span",  x ~ formattable::currency(round(x,-2), symbol="£", digits=0, big.mark=","))
))


# Cross tabulation, broken down by category

df <- cost_model$cost_dataframe
df
library(rpivotTable)
rpivotTable(df)
df %>%
  group_by("category_1") %>%


# Cross tabulation, by week, columnar table?






vl

vegalite::to_spec(vl)

data <- cost_model$cost_dataframe %>%
  filter(date == as.Date("2017-06-04"))
data

vegalite::vegalite() %>%
  vegalite::cell_size(width=500) %>%
  vegalite::add_data(data) %>%
  vegalite::encode_y("category_3", type="nominal") %>%
  vegalite::encode_x("cost_gbp_nominal", aggregate="sum") %>%
  vegalite::mark_bar()



cost_model$cost_dataframe


df <- cost_model$cumcost_dataframe
df
library(dplyr)



df <- df %>%
  arrange(category_1, category_2, date)

vegalite::vegalite(renderer = "svg") %>%
  vegalite::cell_size(height = 500, width=500) %>%
  vegalite::add_data(cost_model$cumcost_dataframe) %>%
  vegalite::encode_x("date", type="temporal") %>%
  vegalite::encode_y("csum_nominal_gbp", aggregate="mean") %>%
  vegalite::timeunit_x("yearmonth") %>%
  vegalite::encode_color("category_1", type="nominal") %>%
  vegalite::axis_y(title="Cumulative cost (nominal, £)") %>%
  vegalite::axis_x(title="Date") %>%
  vegalite::mark_line() -> vl


vl

  spec <- vegalite::to_spec(vl2)
  write_file(spec, 'del.txt')
  cost_model$cumcost_dataframe

df <- cost_model$cumcost_dataframe %>%
  select(category_2, date, csum_nominal_gbp) %>%
    arrange(category_2, date)

df2 <- df %>% filter(category_2 == "technical")

df2 <- df2 %>%
  mutate(date = lubridate::as_datetime(date)) %>%
  filter(date < as.Date("2017-03-01"))

diff(df2$csum_nominal_gbp)

vegalite::vegalite() %>%
  vegalite::add_data(df2) %>%
  vegalite::encode_x("date", type="temporal") %>%
  vegalite::encode_y("csum_nominal_gbp", type="quantitative") %>%
  vegalite::scale_x_time_vl(nice="month") %>%
  vegalite::timeunit_x("daymonthyear") %>%
  vegalite::encode_color("category_2", type="nominal") %>%
  vegalite::mark_line()

library(ggvis)

cost_model$cumcost_dataframe %>%
  ggvis(x = ~date, y = ~ csum_nominal_gbp, stroke = ~category_1) %>%
  group_by(category_1) %>%
  layer_paths(strokeWidth :=3)



cost_model$cumcost_dataframe %>%
  ggvis(x = ~date, y = ~ csum_nominal_gbp, fill = ~category_2) %>%
  group_by(category_2) %>%
  layer_ribbons(y = ~from, y2 = ~to, fill = ~category_2) %>%


library(dplyr)
library(ggvis)
cost_model$cumcost_dataframe %>%
  group_by(date) %>%
  mutate(to = cumsum(csum_nominal_gbp), from = c(0, to[-n()])) %>%
  ggvis(x=~date, fill=~category_2) %>%
  group_by(category_2) %>%
  layer_ribbons(y = ~from, y2 = ~to)



# Extract cost dataframe from model
cost_model$cost_dataframe

shiny_vis(cost_model)

readr::write_csv(df, "delete.csv")

# Problem is we want cumulative costs in each category




periodicity = "week"
lookup <- list()
lookup[["week"]] <- "Week %W %Y"
lookup[["month"]]<- "%b %Y"

this_format <- lookup[[periodicity]]
print(periodicity)
print(this_format)

df %>%
  dplyr::select(date, cost_gbp_nominal) %>%
  dplyr::group_by(date= format(date, format=this_format)) %>%
  dplyr::summarise(sum = sum(cost_gbp_nominal)) %>%
  dplyr::arrange(as.Date(paste("01", date),format=paste("%d", this_format)))

paste("%d", this_format)

as.Date("01 Week 07 2017", "%d Week %W %Y")



devtools::document()
roxygen2::roxygenise()
devtools::build()

df <- cost_model$cost_dataframe %>%
  select(cost, date) %>%
  group_by(week = as.Date(cut(date, "week"))) %>%
  summarise(cost = sum(cost))

df


a <- list(x = "a")
names(a)

("x" %in% names(a))
