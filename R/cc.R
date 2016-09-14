#' Calculate table for case control data
#'
#' This function gives a table of results for case control data.
#' The results are comparable to the cc function in Stata.
#'
#' @param data data frame we are working on
#' @param case.var string of case variable
#' @param exposure.var string of exposure variable
#' @param output type of table output ( e.g. "plain", "html")
#' @param digits number of digits to show after the decimal point
#' @param alpha alpha for confidence intervals
#' @return a list of tables of results and statistics
#'    \itemize{
#'    \item contains TODO
#'    }
#'    The default output="plain" returns two matrices with
#'    the results for further processing.
#'    If option output = "html" is chosen, an html-table is returned.
#'    The table can be shown in Rmarkdown documents by including it in a
#'    chunk with option ' results="asis" ' .
#'
#' @keywords epidemiology, case control
#' @export
cc <- function(data, case.var, exposure.var,
               output = "plain",
               digits = 2,
               alpha = 0.05) {
    # TODO calculate attr. frac. pop

    #TODO implement non-standard evaluation to avoid strings

    stopifnot(is.character(case.var),
              is.character(exposure.var))

    cases <- data[,case.var]
    exposure <- data[,exposure.var]

    results <- list()

    # check if cases and exposure are encoded 1/0
    .binary_check(cases, exposure)

    table <- stats::addmargins(table(cases, exposure))
    prop.exposed <- table[,2]/table[,3]
    table <- cbind(table, prop.exposed)
    colnames(table) <- c("Not exposed", "Exposed",
                         "Total", "Proportion exposed")
    rownames(table) <- c("Controls", "Cases", "Total")
    results$table <- table

    # calculate odds ratio and associated confidence intervals
    # (default is Fisher exact so far)
    or.list <- .or.fct(table[1:2,1:2],
                       ci.type = "exact",
                       alpha = alpha)
    odds.ratio <- or.list$value
    or.ci <- or.list$conf.int

    # calculate attributable fraction among exposed
    attr.frac.ex <- 1 - 1/odds.ratio

    # calculate chi-squared statistic
    chi.squared.result <- stats::chisq.test(table[1:2,1:2],
                                     correct = F)
    p.value <- chi.squared.result$p.value
    chi.squared <- chi.squared.result$statistic

    stats <- rbind(
                        c(odds.ratio, or.ci),
                        c(attr.frac.ex, NA,NA))
    colnames(stats) <-
        c("Value", "CI.lower", "CI.upper")
    rownames(stats) <-
        c("Odds ratio", "Attr. frac. exp.")
    results$stats <- stats
    chi2 <- c(chi.squared = chi.squared,
              p.value = p.value)
    names(chi2) <- c("Chi Squared", "P value")
    results$chi2 <- chi2
    attr(results, "exposure") <- exposure.var
    # return output
    if(output == "plain") {
        print(results, digits = digits)
        return(invisible(results))
    } else if (output == "silent") {
        return(results)
    } else if (output == "html") {
        html.results <- list()
        html.results$table <-
            stargazer::stargazer(results$table,
                                 type ="html", digits = digits)
        cat("\n")
        html.results$stats <-
            stargazer::stargazer(results$stats,
                                 type = "html", digits = digits)
        cat("\n")
        html.results$chi2 <-
            stargazer::stargazer(results$chi2,
                                 type = "html",
                                 digits = digits)
        return(invisible(results))
    } else {
        stop("Output type not implemented!")
    }

}
