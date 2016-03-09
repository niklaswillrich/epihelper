test_that("ir function works as expected", {
    # regression test to check the results of the function
    # do not change.
    data <- readRDS("testdata/irtest.Rds")
    df <- data$df
    table <- data$table
    stats <- data$stats
    results <- ir(df$cases,df$exposure,df$time)
    expect_equal(results$table, table)
    expect_equal(results$stats, stats)
    })
