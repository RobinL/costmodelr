# install.packages("devtools")
# install.packages("dplyr")
# install.pacakges("lubridate")
# install.packages("devtools")
# install.package("readr)
# library(devtools)
# install_github("RobinL/costmodelr")

# detach("package:costmodelr", unload = TRUE)
library(costmodelr)
library(dplyr)
library(lubridate)
library(readr)


key_dates <- readr::read_csv(system.file("extdata", "key_dates_1.csv", package="costmodelr"), col_types=readr::cols())
key_dates[1,1]
recurring_costs =  readr::read_csv(system.file("extdata", "recurring_cost_1.csv", package="costmodelr"), col_types=readr::cols())

staff_utilisation <- readr::read_csv(system.file("extdata", "staff_utilisation_1.csv", package="costmodelr"), col_types=readr::cols())

rate_card <- readr::read_csv(system.file("extdata", "rate_card_1.csv", package="costmodelr"), col_types=readr::cols())

user_variable_costs <- readr::read_csv(system.file("extdata", "user_variable_costs_1.csv", package="costmodelr"), col_types=readr::cols())
users <- readr::read_csv(system.file("extdata", "users_1.csv", package="costmodelr"), col_types=readr::cols())

oneoff_costs <- readr::read_csv(system.file("extdata", "oneoff_costs_1.csv", package="costmodelr"), col_types=readr::cols())




cost_model <- add_oneoff_costs(cost_model, oneoff_costs)
cost_model <- add_recurring_cost(cost_model, recurring_costs)
cost_model <- run_cost_model(cost_model)



cost_model$cost_dataframe


write.csv(cost_model$cost_dataframe,file = "inst/extdata/vignette_example_target_data.csv")



df <- cost_model$cost_dataframe %>%
  select(cost, date) %>%
  group_by(week = as.Date(cut(date, "week"))) %>%
  summarise(cost = sum(cost))

df

# Break down total costs by category
cost_model$cost_dataframe %>%
  select(cost, category_1, category_2, category_3) %>%
  group_by(category_1, category_2, category_3) %>%
  summarise(cost = sum(cost))


# Find max quantity in each category

cost_model$cost_dataframe %>%
  select(quantity, category_1, category_2, category_3) %>%
  group_by(category_1, category_2, category_3) %>%
  summarise(max_quantity = max(quantity))
