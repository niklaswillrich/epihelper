# tests for the cs function
test_that("cs function works as expected", {
    # regression test to check the results of the function
    # do not change.
    data <- readRDS("testdata/cstest.Rds")

    sink(tempfile())  # blocks printing of results
    results <- cs(data$df, "ill", "tira")
    sink()
    # check if the contingeny tables with margins
    # are the same
    expect_equal(results$table, data$table)
    # check if the values for the risk are the same
    expect_equivalent(results$risk, data$risk)
    # for the stats we ignore the attributes and
    # just compare the values
    # check if the OR and other stats are the same
    expect_equivalent(results$stats, data$stats)
    # check if the chi-squared value and p-value ( without
    # continuity correction) are the same
    expect_equivalent(results$chi2, data$chi2)
})
