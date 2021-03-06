#' Calculate table for cohort study data
#'
#' This function gives a table of results for cohort data.
#' The results are comparable to the cs function in Stata.
#'
#' @param data data frame we are working on
#' @param cases string of case variable
#' @param exposure string of exposure variable
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
#' @keywords epidemiology, cohort, relative risks
#' @export

cs <- function(data, case.var, exposure.var,
               strata = NULL,
               output = "plain",
               digits = 2,
               alpha = 0.05) {

    stopifnot(is.character(case.var),
              is.character(exposure.var))

    cases <- data[,case.var]
    exposure <- data[,exposure.var]

    results <- list()
    .binary_check(cases, exposure)
    # calculate the contingency table with margins
    table <- stats::addmargins(table(cases, exposure))
    colnames(table) <- c("Not exposed", "Exposed",
                         "Total")
    rownames(table) <- c("Controls", "Cases", "Total")
    results$table <- table

    # calculate the risks for different cases
    results$risk <- table["Cases",]/table["Total",]
    rd <- .rd.fct(table[1:2,1:2],
                  ci.type = "wald",
                  alpha = alpha)
    rr <- .rr.fct(table[1:2,1:2],
                  ci.type = "wald",
                  alpha = alpha)
    attr.frac.ex <- 1 - results$risk[1]/results$risk[2]
    # calculate risk diff, risk ratio and attr. frac. exp.

    stats <- rbind(c(rd$value, rd$conf.int),
                           c(rr$value, rr$conf.int),
                           c(attr.frac.ex, NA, NA))
    colnames(stats) <-
        c("Value", "CI.lower", "CI.upper")
    rownames(stats) <-
        c("Risk diff.", "Risk ratio", "Attr. frac. exp.")
    results$stats <- stats

    # calculate chi-squared statistic
    chi.squared.result <- stats::chisq.test(table[1:2,1:2],
                                     correct = F)
    p.value <- chi.squared.result$p.value
    chi.squared <- chi.squared.result$statistic

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
                                 type ="html",
                                 digits = digits)
        html.results$stats <-
            stargazer::stargazer(results$stats, type = "html",
                                 digits = digits)
        html.results$chi2 <-
            stargazer::stargazer(results$chi2, type = "html",
                                 digits = digits)
        return(invisible(results))
    } else {
        stop("Output type not implemented!")
    }
}
