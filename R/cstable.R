#' Calculate table for case control data for multiple exposures
#'
#' @param data data frame we are working on
#' @param cases string of case variable
#' @param exposure vector of strings of exposure variables
#' @param output type of table output ( e.g. "plain", "html", "fancy")
#' @param digits number of digits after decimal point in tables and printed output
#' @return table of results and statistics
#'    If option output = "html" is chosen, then an html
#'    table is printed. The default output="plain" returns a matrix with
#'    the results for further processing. If output ="fancy", the function
#'    returns a DT-datatable with interactive features.
#'
#' @keywords epidemiology, case control, table
#' @export
cstable <- function(data, case.var, exposure.vars,
                    output = "plain", digits = 2) {
  stopifnot(is.character(case.var),
            all(sapply(exposure.vars, is.character)))
    results.list <- plyr::llply(exposure.vars,
                                function(exposure.var)
                                    cs(data,case.var, exposure.var,
                                       output = "silent"))

    # create data frame with the relevant values
    results.df <-
        plyr::ldply(results.list,
                          function(results) {
                              c(attr(results, "exposure"),
                                results$table["Total","Exposed"],
                                results$table["Cases","Exposed"],
                                results$risk["Exposed"],
                                results$table["Total","Not exposed"],
                                results$table["Cases","Not exposed"],
                                results$risk["Not exposed"],
                                results$stats["Risk ratio",],
                                results$chi2["P value"]
                              )
                          })
    names(results.df) <- c("Exposure", "Exposed", "Cases exposed",
                           "AR exp.",
                           "Not exposed", "Cases not exposed",
                           "AR not exp.",
                           "Risk ratio", "RR CI.lower", "RR CI.upper",
                           "P.value")
    results.df[, 2:(dim(results.df)[2])] <-
        sapply(results.df[, 2:(dim(results.df)[2])], as.numeric)
    results.df <- results.df %>% dplyr::arrange(P.value)
    # order by lowest p-value
    # return output
    if(output == "plain") {
        print(results.df, digits = digits)
        return(invisible(results.df))
    } else if (output == "html") {
        results.df.sg <- results.df
        results.df.sg$P.value <- format(results.df.sg$P.value,
                                        digits = 3)
        stargazer::stargazer(results.df.sg,
                             type ="html", summary = F,
                             digits = digits)
        return(invisible(results.df))
    } else {
        stop("Output type not implemented!")
    }
}
