context("test full example 1")

test_that("test full example 1", {

    key_dates <- readr::read_csv(system.file("extdata", "key_dates_1.csv", package="costmodelr"), col_types=readr::cols())
    recurring_cost_assumptions =  readr::read_csv(system.file("extdata", "recurring_cost_1.csv", package="costmodelr"), col_types=readr::cols())

    expect_warning(staff_utilisation <- readr::read_csv(system.file("extdata", "staff_utilisation_1.csv", package="costmodelr"), col_types=readr::cols()))

    rate_card <- readr::read_csv(system.file("extdata", "rate_card_1.csv", package="costmodelr"), col_types=readr::cols())

    user_variable_costs <- readr::read_csv(system.file("extdata", "user_variable_costs_1.csv", package="costmodelr"), col_types=readr::cols())
    users <- readr::read_csv(system.file("extdata", "users_1.csv", package="costmodelr"), col_types=readr::cols())

    oneoff <- readr::read_csv(system.file("extdata", "oneoff_costs_1.csv", package="costmodelr"), col_types=readr::cols())
    cost_model <- create_cost_model(key_dates)

    cost_model <- add_recurring_cost(cost_model, recurring_cost_assumptions) %>%
                  add_staff_utilisation(staff_utilisation, rate_card) %>%
                  add_user_variable_costs(users, user_variable_costs) %>%
                  add_oneoff_costs(oneoff) %>%
                  run_cost_model()

    # Deliberately re-run model to make sure this doesn't cause a problem
    cost_model <- run_cost_model(cost_model)

    to_cast <- cost_model$cost_dataframe[,c("date", "cost_gbp_real", "category_3")]

    same_format_as_expected_output <- reshape2::dcast(to_cast, "date ~ category_3", sum, value.var = "cost_gbp_real",na.rm=TRUE)

    expected_answer =  readr::read_csv(system.file("extdata", "expected_answer_1.csv", package="costmodelr"), col_types=readr::cols())
    drop_cols <- colnames(expected_answer)[grepl("working",colnames(expected_answer))]
    drop_cols <- c("date", drop_cols)
    ea <- remove_named_cols_from_df(expected_answer, drop_cols)
    total <- sum(colSums(ea, TRUE))
    expect_equal(sum(cost_model$cost_dataframe$cost_gbp_real),total, tolerance=0.01, scale=1)  #Tolerance of 1 penny

    # Check that if we add everything twice, the costs double
    cost_model <- create_cost_model(key_dates) %>%
      add_recurring_cost(recurring_cost_assumptions) %>%
      add_recurring_cost(recurring_cost_assumptions) %>%
      add_staff_utilisation(staff_utilisation, rate_card) %>%
      add_staff_utilisation(staff_utilisation, rate_card) %>%
      add_user_variable_costs(users, user_variable_costs) %>%
      add_user_variable_costs(users, user_variable_costs) %>%
      add_oneoff_costs(oneoff) %>%
      add_oneoff_costs(oneoff) %>%
      run_cost_model() %>%  # Deliberately run it twice to ensure this doesn't mess up/duplicate results
      run_cost_model()

    expected_answer =  readr::read_csv(system.file("extdata", "expected_answer_1.csv", package="costmodelr"), col_types=readr::cols())
    drop_cols <- colnames(expected_answer)[grepl("working",colnames(expected_answer))]
    drop_cols <- c("date", drop_cols)
    ea <- remove_named_cols_from_df(expected_answer, drop_cols)
    total <- sum(colSums(ea, TRUE)) * 2
    expect_equal(sum(cost_model$cost_dataframe$cost_gbp_real),total, tolerance=0.01, scale=1)  #Tolerance of 1 penny

})






