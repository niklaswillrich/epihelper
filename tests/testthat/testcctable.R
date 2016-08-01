# tests for the cctable function
test_that("cctable function works as expected", {
    # regression test to check the results of the function
    # do not change.
    data <- readRDS("testdata/cctabletest.Rds")

    sink(tempfile())
    results.df <- cctable(data$df, "ill",
                  c("tira","dmousse", "beer"))
    sink()
    # check if the data frame produced by the function is
    # the one expected
    expect_equal(results.df, data$results.df)
})
