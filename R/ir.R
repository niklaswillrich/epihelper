#' Calculate table for incidence data
#'
#' This function gives a table of results for incidence data.
#' The results are comparable to the ir function in Stata.
#'
#' @param data data frame we are working on
#' @param cases string of case variable
#' @param exposure string of exposure variable
#' @param time time at risk for subjects
#' @param output type of table output ( e.g. "plain", "rmarkdown")
#' @return a list of tables of results and statistics
#'    \itemize{
#'    \item table: contains table of cases, time at risk and incidence
#'          split along category exposed/unexposed.
#'    \item stats: contains difference of ir between exposed/unexposed,
#'    ir ratios between exposed/unexposed, attributable fraction exposed
#'    and attributable fraction of population.
#'    }
#'    If option output = "rmarkdown" is chosen, then no return but markdown
#'    tables are printed. The default output="plain" returns two matrices with
#'    the results for further processing.
#'
#' @keywords epidemiology, incidence rate
#' @export
ir <- function(cases, exposure, time,
               output = "plain") {
  # TODO add confidence intervals for estimated characteristics
  # TODO add sanity checks and more options for displaying tables
  # TODO add option for stratifying by some other variable

  n.exact <- 200 # critical value of observations below
                 # which to choose the exact method.
  alpha <- 0.05 # confidence level for intervals


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
  attr.frac.pop <- 1- inc.rate0/inc.rate.total

  inc.diff.var <- (case1/time1^2 + case0/time0^2)^(1/2)
  ci.inc.diff <- c(
        inc.rate1-inc.rate0 + qnorm(alpha/2)*inc.diff.var,
        inc.rate1-inc.rate0 + qnorm(1-alpha/2)*inc.diff.var
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
          exp(log(inc.ratio) + qnorm(0.025)*(1/case1+1/case0)),
          exp(log(inc.ratio) + qnorm(0.975)*(1/case1+1/case0)))

  }



  stats <- rbind(c(inc.diff,ci.inc.diff),
            c(inc.ratio, ci.inc.ratio),
            c(attr.frac.ex,"-", "-"),
            c(attr.frac.pop, "-", "-"))
  rownames(stats) <-
    c("IR difference", "IR ratio",
      "Attr. frac. exposed", "Attr. frac. pop.")
  results <- list(
    table=table,
    stats=stats
  )
  if (output == "plain") {
    return(results)
  } else if (output == "rmarkdown") {
    pander::pandoc.table(table, style="rmarkdown")
    cat("\n")
    pander::pandoc.table(stats, style="rmarkdown")
  } else {
    stop("Output option not recognized.")
  }
}
