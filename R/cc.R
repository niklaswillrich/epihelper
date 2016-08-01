#' Calculate table for case control data
#'
#' This function gives a table of results for case control data.
#' The results are comparable to the cc function in Stata.
#'
#' @param data data frame we are working on
#' @param cases string of case variable
#' @param exposure string of exposure variable
#' @param output type of table output ( e.g. "plain", "html")
#' @return a list of tables of results and statistics
#'    \itemize{
#'    \item contains TODO
#'    }
#'    If option output = "html" is chosen, then no return but html
#'    tables are printed. The default output="plain" returns two matrices with
#'    the results for further processing.
#'
#' @keywords epidemiology, case control
#' @export
cc <- function(data, case.var, exposure.var,
               output = "plain", digits = 2) {
    # TODO calculate attr. frac. pop

    #TODO implement non-standard evaluation to avoid strings
    # case.var <- substitute(case.var)
    # cases <- eval(quote(case.var), data)
    # exposure.var <- substitute(exposure.var)
    # exposure <- eval(quote(exposure.var), data)
    stopifnot(is.character(case.var),
              is.character(exposure.var))

    cases <- data[,case.var]
    exposure <- data[,exposure.var]

    results <- list()

    # check if cases and exposure are encoded 1/0
    .binary.check(cases, exposure)

    table <- addmargins(table(cases, exposure))
    prop.exposed <- table[,2]/table[,3]
    table <- cbind(table, prop.exposed)
    colnames(table) <- c("Not exposed", "Exposed",
                         "Total", "Proportion exposed")
    rownames(table) <- c("Controls", "Cases", "Total")
    results$table <- table

    # calculate odds ratio and associated confidence intervals
    # (default is Fisher exact so far)
    or.list <- .or.fct(table[1:2,1:2])
    odds.ratio <- or.list$value
    or.ci <- or.list$conf.int

    # calculate attributable fraction among exposed
    attr.frac.ex <- 1 - 1/odds.ratio

    # calculate chi-squared statistic
    chi.squared.result <- chisq.test(table[1:2,1:2],
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
