# tests for the cctable function
test_that("cstable function works as expected", {
    # regression test to check the results of the function
    # do not change.
    data <- readRDS("testdata/cstabletest.Rds")

    sink(tempfile())  # blocks printing of results
    results.df <- cstable(data$df, "ill",
                  c("tira","dmousse", "beer"))
    sink()

    # check if the data frame produced by the function is
    # the one expected
    expect_equal(results.df, data$results.df)
})
