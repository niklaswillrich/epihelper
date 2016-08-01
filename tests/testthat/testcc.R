# tests for the cc function
test_that("cc function works as expected", {
    # regression test to check the results of the function
    # do not change.
    data <- readRDS("testdata/cctest.Rds")

    sink(tempfile())  # blocks printing of results
    results <- cc(data$df, "ill", "tira")
    sink()
    # check if the contingeny tables with margins
    # are the same
    expect_equal(results$table, data$table)
    # for the stats we ignore the attributes and
    # just compare the values
    # check if the OR and other stats are the same
    expect_equivalent(results$stats, data$stats)
    # check if the chi-squared value and p-value ( without
    # continuity correction) are the same
    expect_equivalent(results$chi2, data$chi2)
})
