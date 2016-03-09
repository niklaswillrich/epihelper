#' Calculate table for incidence data
#'
#' This function gives a table of results for incidence data
#'
#' @param data data frame we are working on
#' @param cases string of case variable
#' @param exposure string of exposure variable
#' @param time time of exposure for subjects
#' @param output type of table output ( e.g. "plain", "rmarkdown")
#' @return tables of results and statistics
#'    table contains table of cases, time at risk and incidence rate
#'    split along category exposed/unexposed.
#'    If option output = "rmarkdown", then no return but markdown
#'    tables are printed.
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


  inc.rate0 <- case0/time0
  inc.rate1 <- case1/time1
  inc.rate.total <- case.total/time.total

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
    pandoc.table(table, style="rmarkdown")
    cat("\n")
    pandoc.table(stats, style="rmarkdown")
  } else {
    stop("Output option not recognized.")
  }
}
