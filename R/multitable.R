#' Show tables of multiple models
#' this is a wrapper function for producing nice
#' output tables with the stargazer package adapted
#'  to the needs of an epidemiologist.
#'
#'
#'


multitable <- function(..., padding="3px") {
# todo use stargazer to get a table
    stargazer(...)
}
