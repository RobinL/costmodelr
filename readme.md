Introduction
------------

This vignette presents an example of how to use the `costmodelr` package. You can access all the vignettes for this package in R by typing `browseVignettes("costmodelr")`.

You can download a template of a cost model with working code [here](https://github.com/RobinL/costmodelr_project_template)

Package basics
--------------

The `costmodelr` package provides a set of utility functions for turning a set of cost assumptions into a [tidy](http://vita.had.co.nz/papers/tidy-data.pdf) table that has one row for each cost on each date that cost is incurred.

The core output format has just five columns as follows:

| date       | id       |  quantity|  price\_gbp| real\_or\_nominal |
|:-----------|:---------|---------:|-----------:|:------------------|
| 2017-01-02 | oo\_0\_1 |         4|          50| real              |
| 2017-01-04 | oo\_0\_2 |         2|         100| real              |
| 2017-01-10 | oo\_0\_3 |         1|         150| real              |

Additional information is associated with each cost that enables final results to be filtered and cross tabulated. This can then be joined onto the core output to produce a dataframe like this:

| date       | id       |  quantity| category\_1 | category\_2 | category\_3 |  gdp\_deflator|  green\_book\_discount|  cost\_gbp\_real|  cost\_gbp\_nominal| period |
|:-----------|:---------|---------:|:------------|:------------|:------------|--------------:|----------------------:|----------------:|-------------------:|:-------|
| 2017-01-02 | oo\_0\_1 |         4| a           | b           | c           |       1.028260|               1.063935|              200|            205.6521| alpha  |
| 2017-01-04 | oo\_0\_2 |         2| a           | b           | d           |       1.028373|               1.064135|              200|            205.6747| alpha  |
| 2017-01-10 | oo\_0\_3 |         1| b           | b           | d           |       1.028713|               1.064737|              150|            154.3069| live   |

This output can be accessed from `cost_model$cost_dataframe`.

The categorisation columns are provided by the user and can be altered from the defaults using `costmodelr::setting_categorisation_columns()` or `setting_append_to_categorisation_columns()`.

Since this output dataframe is tidy, it is easy to perform aggregations and filtering, and produce tabular and graphical output that summarises actual and forecasted costs.

Assumptions are input into the model from dataframes, typically read in from `.csv` files, which must be provided in a specific format. There are a number of different assumption types, such as 'one off costs', recurring costs' and 'staff costs'.

Assumption types
----------------

### Key dates

The cost model should be iniitalised with 'key dates', which control the time period over which outputs will be generated.

Key dates look like this:

| date       | period |
|:-----------|:-------|
| 2017-01-01 | alpha  |
| 2017-01-05 | beta   |
| 2017-01-10 | live   |

In addition to a 'date' columns, any other columns in the key dates dataframe will be added to the final outputs, and therefore can be used for cross tabulation.

### One off costs

One off costs are costs that occur only once. `costmodelr` does not need to perform complex computations on these assupmptions, and so the input is similar to the output; there is a one to one correspondence between the rows in the assumption file and output rows.

The input format is as follows:

|  price\_in\_original\_currency| real\_or\_nominal | currency |  quantity| date     | category\_1 | category\_2 | category\_3 | Description | Source |
|------------------------------:|:------------------|:---------|---------:|:---------|:------------|:------------|:------------|:------------|:-------|
|                             50| real              | GBP      |         4| 02/01/17 | a           | b           | c           | abc         | x      |
|                            100| real              | GBP      |         2| 04/01/17 | a           | b           | d           | acbd        | y      |
|                            150| real              | GBP      |         1| 10/01/17 | b           | b           | d           | bbd         | z      |

The output would look as follows:

| date       | id       |  quantity| category\_1 | category\_2 | category\_3 |  gdp\_deflator|  green\_book\_discount|  cost\_gbp\_real|  cost\_gbp\_nominal| period |
|:-----------|:---------|---------:|:------------|:------------|:------------|--------------:|----------------------:|----------------:|-------------------:|:-------|
| 2017-01-02 | oo\_0\_1 |         4| a           | b           | c           |       1.028260|               1.063935|              200|            205.6521| alpha  |
| 2017-01-04 | oo\_0\_2 |         2| a           | b           | d           |       1.028373|               1.064135|              200|            205.6747| alpha  |
| 2017-01-10 | oo\_0\_3 |         1| b           | b           | d           |       1.028713|               1.064737|              150|            154.3069| live   |

### Recurring costs

Allows you to model costs that happen at a given frequency. Includes options that allow costs to grow at given % and fixed rates.

The input format is as follows:

|  price\_in\_original\_currency| real\_or\_nominal | currency |  quantity| frequency | first\_date |  growth\_in\_cost\_percent\_per\_annum|  growth\_in\_cost\_absolute\_per\_annum|  growth\_in\_quantity\_percent\_per\_annum|  growth\_in\_quantity\_absolute\_per\_annum| category\_1 | category\_2 | category\_3 |
|------------------------------:|:------------------|:---------|---------:|:----------|:------------|--------------------------------------:|---------------------------------------:|------------------------------------------:|-------------------------------------------:|:------------|:------------|:------------|
|                           1000| real              | GBP      |         1| day       | 2017-01-01  |                               36.87754|                                       0|                                          0|                                        0.00| capital     | hardware    | cpu         |
|                           1000| real              | GBP      |         2| day       | 2017-01-09  |                                0.00000|                                       0|                                          0|                                      365.25| capital     | software    | analysis    |

The output would look as follows:

| date       | id       |  quantity| category\_1 | category\_2 | category\_3 |  gdp\_deflator|  green\_book\_discount|  cost\_gbp\_real|  cost\_gbp\_nominal| period |
|:-----------|:---------|---------:|:------------|:------------|:------------|--------------:|----------------------:|----------------:|-------------------:|:-------|
| 2017-01-01 | rc\_0\_1 |         1| capital     | hardware    | cpu         |       1.028204|               1.063835|         1000.000|            1028.204| alpha  |
| 2017-01-02 | rc\_0\_1 |         1| capital     | hardware    | cpu         |       1.028260|               1.063935|         1010.000|            1038.543| alpha  |
| 2017-01-03 | rc\_0\_1 |         1| capital     | hardware    | cpu         |       1.028317|               1.064035|         1020.100|            1048.986| alpha  |
| 2017-01-04 | rc\_0\_1 |         1| capital     | hardware    | cpu         |       1.028373|               1.064135|         1030.301|            1059.534| alpha  |
| 2017-01-05 | rc\_0\_1 |         1| capital     | hardware    | cpu         |       1.028430|               1.064236|         1040.604|            1070.188| beta   |
| 2017-01-06 | rc\_0\_1 |         1| capital     | hardware    | cpu         |       1.028487|               1.064336|         1051.010|            1080.950| beta   |
| 2017-01-07 | rc\_0\_1 |         1| capital     | hardware    | cpu         |       1.028543|               1.064436|         1061.520|            1091.819| beta   |
| 2017-01-08 | rc\_0\_1 |         1| capital     | hardware    | cpu         |       1.028600|               1.064536|         1072.135|            1102.798| beta   |
| 2017-01-09 | rc\_0\_1 |         1| capital     | hardware    | cpu         |       1.028656|               1.064637|         1082.857|            1113.887| beta   |
| 2017-01-10 | rc\_0\_1 |         1| capital     | hardware    | cpu         |       1.028713|               1.064737|         1093.685|            1125.088| live   |
| 2017-01-09 | rc\_0\_2 |         2| capital     | software    | analysis    |       1.028656|               1.064637|         2000.000|            2057.313| beta   |
| 2017-01-10 | rc\_0\_2 |         3| capital     | software    | analysis    |       1.028713|               1.064737|         3000.000|            3086.139| live   |

### Staff utilisation

Allows you to model staff costs, given assumptions of staff % utilisation on the project.

Two different sets of assumptions are needed here: % utilisation, and a ratecard.

The ratecard looks like this

|  price\_in\_original\_currency| real\_or\_nominal | id  | currency | price\_frequency |  annual\_percentage\_increase| category\_1 | category\_2   | category\_3         |
|------------------------------:|:------------------|:----|:---------|:-----------------|-----------------------------:|:------------|:--------------|:--------------------|
|                             20| real              | TA  | GBP      | working\_day     |                             0| staff       | technical     | technical architect |
|                             10| real              | PM  | GBP      | working\_day     |                             0| staff       | non technical | product manager     |

The staff utilisation assumptions look like this:

    ## Warning: Duplicated column names deduplicated: 'TA' => 'TA_1' [3]

| date       |   TA|  TA\_1|    PM|
|:-----------|----:|------:|-----:|
| 2017-01-01 |  0.5|      1|  0.25|
| 2017-01-05 |  1.0|      1|  0.25|
| 2017-01-10 |  0.5|      1|  0.25|

The output looks like this:

| date       | id        |  quantity| category\_1 | category\_2 | category\_3         |  gdp\_deflator|  green\_book\_discount|  cost\_gbp\_real|  cost\_gbp\_nominal| period |
|:-----------|:----------|---------:|:------------|:------------|:--------------------|--------------:|----------------------:|----------------:|-------------------:|:-------|
| 2017-01-01 | su\_0\_TA |       0.5| staff       | technical   | technical architect |       1.028204|               1.063835|         7.142857|            7.344312| alpha  |
| 2017-01-02 | su\_0\_TA |       0.5| staff       | technical   | technical architect |       1.028260|               1.063935|         7.142857|            7.344716| alpha  |
| 2017-01-03 | su\_0\_TA |       0.5| staff       | technical   | technical architect |       1.028317|               1.064035|         7.142857|            7.345120| alpha  |
| 2017-01-04 | su\_0\_TA |       0.5| staff       | technical   | technical architect |       1.028373|               1.064135|         7.142857|            7.345524| alpha  |
| 2017-01-05 | su\_0\_TA |       1.0| staff       | technical   | technical architect |       1.028430|               1.064236|        14.285714|           14.691857| beta   |
| 2017-01-06 | su\_0\_TA |       1.0| staff       | technical   | technical architect |       1.028487|               1.064336|        14.285714|           14.692665| beta   |
| 2017-01-07 | su\_0\_TA |       1.0| staff       | technical   | technical architect |       1.028543|               1.064436|        14.285714|           14.693473| beta   |
| 2017-01-08 | su\_0\_TA |       1.0| staff       | technical   | technical architect |       1.028600|               1.064536|        14.285714|           14.694281| beta   |
| 2017-01-09 | su\_0\_TA |       1.0| staff       | technical   | technical architect |       1.028656|               1.064637|        14.285714|           14.695090| beta   |
| 2017-01-10 | su\_0\_TA |       0.5| staff       | technical   | technical architect |       1.028713|               1.064737|         7.142857|            7.347949| live   |

(Note only the first 10 rows of the output are show. Note also costs are spread equally throughout the week, so £50 a week = ~£7.14 a day, including Sat and Sun)

### User variable costs

This type of cost is to deal with costs which are proportional to the number of users.

It's possible to model both costs which are directly proportional to the number of users (e.g. each user needs a Github account), or to deal with costs which grow with the number of users (e.g. each user uses an *additional* 2gb of storage each month)

The input assumptions look like this:

Number of users (this will be linearly interpolated):

| date       |  num\_users|
|:-----------|-----------:|
| 2017-01-01 |           1|
| 2017-01-03 |           1|
| 2017-01-05 |           1|
| 2017-01-06 |           1|
| 2017-01-07 |           2|

Cost assumptions:

|  price\_in\_original\_currency| real\_or\_nominal | currency | pricefrequency |  fixed\_initial\_quantity\_per\_user|  growth\_in\_cost\_percent\_per\_annum|  growth\_in\_cost\_absolute\_per\_annum|  growth\_in\_quantity\_absolute\_per\_annum\_per\_user| category\_1    | category\_2 | category\_3 |
|------------------------------:|:------------------|:---------|:---------------|------------------------------------:|--------------------------------------:|---------------------------------------:|------------------------------------------------------:|:---------------|:------------|:------------|
|                             60| real              | GBP      | month          |                                    0|                                  0.000|                                       0|                                                 365.25| infrastructure | storage     | amazon s3   |
|                             30| real              | GBP      | month          |                                    0|                                  0.000|                                       0|                                                 730.50| infrastructure | compute     | ec2         |
|                              1| real              | GBP      | day            |                                   10|                                  0.000|                                       0|                                                   0.00| infrastructure | accounts    | github      |
|                             14| real              | GBP      | week           |                                  100|                               1383.244|                                       0|                                                 365.25| infrastructure | storate     | ebs         |

The output looks like this:

| date       | id        |  quantity| category\_1    | category\_2 | category\_3 |  gdp\_deflator|  green\_book\_discount|  cost\_gbp\_real|  cost\_gbp\_nominal| period |
|:-----------|:----------|---------:|:---------------|:------------|:------------|--------------:|----------------------:|----------------:|-------------------:|:-------|
| 2017-01-01 | uvc\_0\_1 |         1| infrastructure | storage     | amazon s3   |       1.028204|               1.063835|         1.971253|            2.026849| alpha  |
| 2017-01-02 | uvc\_0\_1 |         2| infrastructure | storage     | amazon s3   |       1.028260|               1.063935|         3.942505|            4.053921| alpha  |
| 2017-01-03 | uvc\_0\_1 |         3| infrastructure | storage     | amazon s3   |       1.028317|               1.064035|         5.913758|            6.081217| alpha  |
| 2017-01-04 | uvc\_0\_1 |         4| infrastructure | storage     | amazon s3   |       1.028373|               1.064135|         7.885010|            8.108735| alpha  |
| 2017-01-05 | uvc\_0\_1 |         5| infrastructure | storage     | amazon s3   |       1.028430|               1.064236|         9.856263|           10.136476| beta   |
| 2017-01-06 | uvc\_0\_1 |         6| infrastructure | storage     | amazon s3   |       1.028487|               1.064336|        11.827515|           12.164440| beta   |
| 2017-01-07 | uvc\_0\_1 |         8| infrastructure | storage     | amazon s3   |       1.028543|               1.064436|        15.770020|           16.220146| beta   |
| 2017-01-08 | uvc\_0\_1 |        10| infrastructure | storage     | amazon s3   |       1.028600|               1.064536|        19.712526|           20.276298| beta   |
| 2017-01-09 | uvc\_0\_1 |        12| infrastructure | storage     | amazon s3   |       1.028656|               1.064637|        23.655031|           24.332896| beta   |
| 2017-01-10 | uvc\_0\_1 |        14| infrastructure | storage     | amazon s3   |       1.028713|               1.064737|        27.597536|           28.389940| live   |

(again, only the first 10 records are shown)

Running a full cost model
-------------------------

Running the full cost model amounts to loading in assumptions, and using the `run cost model` function.

``` r
# The 'key dates' file specifies the time period over which the cost model produces estimates
key_dates <- readr::read_csv("assumptions/key_dates.csv", col_types=readr::cols())

# Read in assumptions from files
users <- readr::read_csv("assumptions/users.csv", col_types=readr::cols())
staff_utilisation <- readr::read_csv("assumptions/staff_utilisation.csv", col_types=readr::cols())
rate_card <- readr::read_csv("assumptions/rate_card.csv", col_types=readr::cols())
recurring_costs <-  readr::read_csv("assumptions/recurring_cost.csv", col_types=readr::cols())
oneoff_costs <- readr::read_csv("assumptions/oneoff_costs.csv", col_types=readr::cols())
user_variable_costs <- readr::read_csv("assumptions/user_variable_costs.csv", col_types =readr::cols())

# Add each set of assumptions to model
cost_model <- create_cost_model(key_dates) %>% 
  add_oneoff_costs(oneoff_costs) %>% 
  add_recurring_cost(recurring_costs) %>%  
  add_user_variable_costs(users, user_variable_costs) %>% 
  add_staff_utilisation(staff_utilisation, rate_card)

# Run model
cost_model <- run_cost_model(cost_model)

# Extract cost dataframe from model - use this if you want to do crosstabs or filters
cost_model$cost_dataframe

# Run a Shiny app to interactively explore your data
shiny_vis(cost_model)

# View a hierarchical bubble chart of the model
shiny_bubble(cost_model)
```
