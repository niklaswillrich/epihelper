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
#' @keywords epidemiology
#' @export
ir <- function(cases, exposure, time,
               output = "plain") {
  # TODO add confidence intervals for estimated characteristics
  # TODO add sanity checks and more options for displaying tables
  # TODO add option for stratifying by some other variable
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
  stats= matrix(c(inc.diff=inc.diff, inc.ratio=inc.ratio,
                  attr.frac.ex=attr.frac.ex,
                  attr.frac.pop=attr.frac.pop), ncol=1)
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
