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

    cost_model <- add_recurring_cost(cost_model, recurring_cost_assumptions)
    cost_model <- add_staff_utilisation(cost_model, staff_utilisation, rate_card)
    cost_model <- add_user_variable_costs(cost_model, users, user_variable_costs)
    cost_model <- add_oneoff_costs(cost_model, oneoff)
    cost_model <- run_cost_model(cost_model)

    chunks <- do.call(rbind, cost_model$chunks)

    chunks$cost <- chunks$price_gbp_real * chunks$quantity
    id_lookup <- do.call(rbind, cost_model$id_lookup)



    all <- chunks %>%
          dplyr::left_join(id_lookup)

    to_cast <- all[,c("date", "cost", "category_3")]

    same_format_as_expected_output <- reshape2::dcast(to_cast, "date ~ category_3", sum, value.var = "cost",na.rm=TRUE)

    expected_answer =  readr::read_csv(system.file("extdata", "expected_answer_1.csv", package="costmodelr"), col_types=readr::cols())
    drop_cols <- colnames(expected_answer)[grepl("working",colnames(expected_answer))]
    drop_cols <- c("date", drop_cols)
    ea <- remove_named_cols_from_df(expected_answer, drop_cols)
    total <- sum(colSums(ea, TRUE))
    expect_equal(sum(chunks$cost),total, tolerance=0.01, scale=1)  #Tolerance of 1 penny
 

})


