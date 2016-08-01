test_that("ir function works as expected", {
    # regression test to check the results of the function
    # do not change.
    data <- readRDS("testdata/irtest.Rds")
    results <- ir(data$df, "cases", "exposure" ,"time")
    expect_equal(results$table, data$table)
    # for the stats we ignore the attributes and
    # just compare the values
    expect_equivalent(results$stats, data$stats)
    })
