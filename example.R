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


# Read in assumptions from files

key_dates <- readr::read_csv("/Users/robinlinacre/Documents/r_projects/final_cost_model/assumptions/key_dates.csv", col_types=readr::cols())
recurring_costs =  readr::read_csv( "/Users/robinlinacre/Documents/r_projects/final_cost_model/assumptions/recurring_cost.csv", col_types=readr::cols())
staff_utilisation <- readr::read_csv( "/Users/robinlinacre/Documents/r_projects/final_cost_model/assumptions/staff_utilisation_forecast.csv", col_types=readr::cols())

rate_card <- readr::read_csv( "/Users/robinlinacre/Documents/r_projects/final_cost_model/assumptions/rate_card_forecast.csv", col_types=readr::cols())

user_variable_costs <- readr::read_csv( "/Users/robinlinacre/Documents/r_projects/final_cost_model/assumptions/user_variable_costs.csv", col_types=readr::cols())
users <- readr::read_csv( "/Users/robinlinacre/Documents/r_projects/final_cost_model/assumptions/users.csv", col_types=readr::cols())

oneoff <- readr::read_csv( "/Users/robinlinacre/Documents/r_projects/final_cost_model/assumptions/oneoff_costs_actual.csv", col_types=readr::cols())


# Add each set of assumptions to model
cost_model <- create_cost_model(key_dates) %>%
  add_oneoff_costs(oneoff) %>%
  add_oneoff_costs(oneoff) %>%
  add_recurring_cost(recurring_costs) %>%
  add_recurring_cost(recurring_costs) %>%
  add_user_variable_costs(users, user_variable_costs) %>%
  add_user_variable_costs(users, user_variable_costs) %>%
  add_staff_utilisation(staff_utilisation, rate_card) %>%
  add_staff_utilisation(staff_utilisation, rate_card)

# Run model
cost_model <- run_cost_model(cost_model)

cost_model$cost_dataframe %>%
  group_by(category_1, category_2, category_3) %>%
  summarise(value = sum(cost_gbp_nominal))

cost_model$cost_dataframe %>%
  arrange(category_3, date)

shiny_vis(cost_model)
shiny_bubble(cost_model)

rmarkdown::render("vignettes/assumption_types.Rmd", output_format="md_document", output_file = "../README.md", output_options=list("variant" = "markdown_github"))
x <- readLines("readme.md")
y <- gsub( "../README_files/f", "../master/README_files/f", x )
y <- c("[![Build Status](https://travis-ci.org/RobinL/costmodelr.svg?branch=master)](https://travis-ci.org/RobinL/costmodelr)",y)
y <- c("[![Coverage Status](https://img.shields.io/codecov/c/github/RobinL/costmodelr/master.svg)](https://codecov.io/github/RobinL/costmodelr?branch=master)",y)



cat(y, file="readme.md", sep="\n")


