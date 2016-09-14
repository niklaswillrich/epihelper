#' Calculate table for incidence data
#'
#' This function gives a table of results for incidence data.
#' The results are comparable to the ir function in Stata.
#'
#' @param data data frame we are working on
#' @param cases string of case variable
#' @param exposure string of exposure variable
#' @param time time at risk for subjects
#' @param output type of table output ( e.g. "plain", "html")
#' @param digits number of digits to show after the decimal point
#' @return a list of tables of results and statistics
#'    \itemize{
#'    \item table: contains table of cases, time at risk and incidence
#'          split along category exposed/unexposed.
#'    \item stats: contains difference of ir between exposed/unexposed,
#'    ir ratios between exposed/unexposed, attributable fraction exposed
#'    and attributable fraction of population.
#'    }
#'    If option output = "html" is chosen, then no return but html
#'    tables are printed. The default output="plain" returns two matrices with
#'    the results for further processing.
#'
#' @keywords epidemiology, incidence rate
#' @export
ir <- function(data, case.var, exposure.var, time.var,
               output = "plain", digits = 2,
               alpha = 0.05) {
  # TODO add confidence intervals for estimated characteristics
  # TODO add sanity checks and more options for displaying tables
  # TODO add option for stratifying by some other variable



  cases <- data[,case.var]
  exposure <- data[,exposure.var]
  time <- data[,time.var]

  case0 <- sum(cases[exposure == 0])
  case1 <- sum(cases[exposure == 1])
  time0 <- sum(time[exposure == 0])
  time1 <- sum(time[exposure == 1])

  case.total <- sum(cases)
  time.total <- sum(time)


  inc.rate0 <- case0/time0 # incidence rate for unexposed subjects
  inc.rate1 <- case1/time1 # incidence rate for exposed subjects
  inc.rate.total <- case.total/time.total # incidence rates for all subjects

  table <- matrix(c(case0, case1, case.total,
                    time0, time1, time.total,
                    inc.rate0, inc.rate1, inc.rate.total), ncol = 3, byrow = T)
  colnames(table) <- c("Not exposed", "Exposed", "Total")
  rownames(table) <- c("Cases", "Time at risk",
                       "Incidence rate")

  inc.diff <- inc.rate1-inc.rate0
  inc.ratio <- inc.rate1/inc.rate0
  attr.frac.ex <- (inc.rate1-inc.rate0)/inc.rate1
  attr.frac.pop <- 1 - inc.rate0/inc.rate.total

  inc.diff.var <- (case1/time1^2 + case0/time0^2)^(1/2)
  ci.inc.diff <- c(
        inc.rate1-inc.rate0 + stats::qnorm(alpha/2)*inc.diff.var,
        inc.rate1-inc.rate0 + stats::qnorm(1-alpha/2)*inc.diff.var
        )

  if (FALSE) {
      # NOT IMPLEMENTED YET
      # TODO implement calculations for exact confidence intervals
    # exact confidence intervals
    # ci.inc.ratio.lower <- uniroot(function(p) 1 - (dbinom(aa, mm, p)/2 +
    #                                                    pbinom(aa-1, mm, p)) -
    #                                                     alpha/2, c(0, 1))$root
    # # poisson approximation
    # ci.inc.ratio <- time0/time1*case1/(case0+1)*1/(qf(alpha/2,2*(case0+1),2*case1))
    #  time0/time1*(case1+1)/case0*1/(qf(1-alpha/2,2*(case1+1),2*case0))

  } else {
      # Wald-type estimation
        ci.inc.ratio <- c(
          exp(log(inc.ratio) + stats::qnorm(alpha/2)*(1/case1+1/case0)),
          exp(log(inc.ratio) + stats::qnorm(1-alpha/2)*(1/case1+1/case0)))

  }



  stats <- rbind(c(inc.diff,ci.inc.diff),
            c(inc.ratio, ci.inc.ratio),
            c(attr.frac.ex,NA, NA),
            c(attr.frac.pop, NA, NA))
  rownames(stats) <-
    c("IR difference", "IR ratio",
      "Attr. frac. exposed", "Attr. frac. pop.")
  colnames(stats) <- c("Value", "CI.lower", "CI.upper")
  results <- list(
    table=table,
    stats=stats
  )
  if (output == "plain") {
      print(results, digits = digits)
      return(invisible(results))
  } else if (output == "silent") {
      return(results)
  } else if (output == "html") {
      stargazer::stargazer(table, type ="html",
                           digits = digits)
      stargazer::stargazer(stats, type ="html",
                           digits = digits, digits.extra = digits)
  } else {
    stop("Output option not recognized.")
  }
}
