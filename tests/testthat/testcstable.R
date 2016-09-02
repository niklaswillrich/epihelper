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
    expect_error(cstable(data$df, 12,
                         c("tira","dmousse", "beer")))
    expect_error(cstable(data$df, 12,
                         c(NA,"dmousse", "beer")))
    expect_error(cstable(data$df, "tportions",
                         c("dmousse", "beer")))
    expect_error(cstable(data$df, "tportion",
                         c("dmousse", "beer")),
                 regexp =".*must be encoded 1/0")
})
