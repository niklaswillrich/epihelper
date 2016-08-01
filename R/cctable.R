#' Calculate table for case control data for multiple exposures
#'
#' @param data data frame we are working on
#' @param cases string of case variable
#' @param exposure vector of strings of exposure variables
#' @param digits number of digits in tables and printed output
#' @param output type of table output ( e.g. "plain", "html")
#' @return table of results and statistics
#'    If option output = "html" is chosen, then no return but html
#'    tables are printed. The default output="plain" returns two matrices with
#'    the results for further processing.
#'
#' @keywords epidemiology, case control, table
#' @export
cctable <- function(data, case.var, exposure.vars,
                    output = "plain", digits = 2) {

    stopifnot(is.character(case.var),
            all(sapply(exposure.vars, is.character)))
    results.list <- plyr::llply(exposure.vars,
          function(exposure.var)
              cc(data,case.var, exposure.var,
                 output = "silent"))

    # create data frame with the relevant values
    results.df <- plyr::ldply(results.list,
                function(results) {
                    c(attr(results, "exposure"),
                      results$table["Cases","Total"],
                      results$table["Cases","Exposed"],
                      results$table["Cases","Proportion exposed"],
                      results$table["Controls","Total"],
                      results$table["Controls","Exposed"],
                      results$table["Controls","Proportion exposed"],
                      results$stats["Odds ratio",],
                      results$chi2["P value"]
                      )
                })
    names(results.df) <- c("Exposure", "Cases total", "Cases exposed",
                           "Cases prop. exp.",
                           "Controls total", "Controls exposed",
                           "Controls prop. exp.",
                           "Odds ratio", "OR CI.lower", "OR CI.upper",
                           "P.value")
    results.df[, 2:dim(results.df)[2]] <-
        sapply(results.df[, 2:dim(results.df)[2]], as.numeric)
    results.df <- results.df %>% dplyr::arrange(P.value)
    # order by lowest p-value
    # return output
    if (output == "plain") {
        print(results.df, digits = digits)
        return(invisible(results.df))
    } else if (output == "html") {
        results.df.sg <- results.df
        results.df.sg$P.value <- format(results.df.sg$P.value,
                                        digits = digits)
        stargazer::stargazer(results.df.sg,
                             type ="html", summary = F,
                             digits = digits)
        return(invisible(results.df))
    } else if (output == "fancy") {
        fancy.table <- DT::datatable(results.df) %>%
            formatRound(c(4,7:(dim(results.df)[2]-1)), 2) %>%            formatStyle(
                'Cases prop. exp.',
                background = styleColorBar(c(0,1), 'lightgreen'),
                backgroundPosition = 'left'
            ) %>%            formatStyle(
                'Controls prop. exp.',
                background = styleColorBar(c(0,1), 'lightgreen')
            )
            format
        return(fancy.table)
    } else {
        stop("Output type not implemented!")
    }
}
