# list of helper functions which are only used internally

# function that calculates odds ratio internally
# Input:
# 2*2 table which is ordered from non-case to case
# and not exposed to exposed
# Output:
# value - odds ratio
# conf.int - confidence interval of type ci.type
# to the level alpha
.or.fct <- function(table,
                    ci.type = "exact",
                    alpha = 0.05) {
    value <- (table[2,2]/table[1,2])/
        (table[2,1]/table[1,1])
    if(ci.type == "exact")
    conf.int <- fisher.test(table[1:2,1:2],
                            conf.level = 1-alpha)$conf.int
    return(list(
        value = value,
        conf.int = conf.int
    ))
}


# function that calculates risk difference internally
# Input:
# 2*2 table which is ordered from non-case to case
# and not exposed to exposed
# Output:
# value - risk difference
# conf.int - confidence interval of type ci.type
# to the level alpha
.rd.fct <- function(table,
                    ci.type = "exact",
                    alpha = 0.05) {
    risk.diff <- table[2,2]/(table[1,2]+table[2,2])-
        table[2,1]/(table[1,1]+table[2,1])
    sd.risk.diff <- (
        table[2,2]*table[1,2]/
        ((table[2,2]+table[1,2])^2*(table[2,2]+table[1,2])) +
        table[2,1]*table[1,1]/
        ((table[2,1]+table[1,1])^2*(table[2,1]+table[1,1]))
        )^(1/2)
    ci.risk.diff.lower <- risk.diff + qnorm(alpha/2)*sd.risk.diff
    ci.risk.diff.upper <- risk.diff + qnorm(1-alpha/2)*sd.risk.diff
    value <- risk.diff
    conf.int <- c(ci.risk.diff.lower, ci.risk.diff.upper)
    return(list(
        value = value,
        conf.int = conf.int
    ))
    }

# function that calculates the value of the risk ratio
# internally
# Input:
# 2*2 table which is ordered from non-case to case
# and not exposed to exposed
# Output:
# value - risk ratio
.rr.value.fct <- function(table) {
    table[2,2]/(table[1,2]+table[2,2])/
        (table[2,1]/(table[1,1]+table[2,1]))
}


# function that calculates risk ratio and confidence intervals internally
# Input:
# 2*2 table which is ordered from non-case to case
# and not exposed to exposed
# Output:
# value - risk ratio
# conf.int - confidence interval of type ci.type
# to the level alpha
.rr.fct <- function(table,
                    ci.type = "exact",
                    alpha = 0.05) {

    risk.ratio <- .rr.value.fct(table)
    sd.log.risk.ratio <- (1/table[2,2] - 1/(table[2,2]+table[1,2]) +
                           1/table[2,1] - 1/(table[2,1]+table[1,1]))^(1/2)

    ci.risk.ratio.lower <- exp(log(risk.ratio) + qnorm(alpha/2)*sd.log.risk.ratio)
    ci.risk.ratio.upper <- exp(log(risk.ratio) + qnorm(1-alpha/2)*sd.log.risk.ratio)
    value <- risk.ratio
    conf.int <- c(ci.risk.ratio.lower, ci.risk.ratio.upper)
    return(list(
        value = value,
        conf.int = conf.int
    ))
}




## function to calculate Mantel-Haenszel odds ratio
.mh.or.fct <- function(cases, exposure, strata) {

}

# function to check which rows of a data frame are
# equal to a given row
check.df.equality <- function(line, df) {
    apply(df,1, function(line.i) all(line == line.i))
}

## function to calculate Mantel-Haenszel risk ratio
## input
## cases - vector of cases coded as 0,1
## exposure - vector of exposure coded as 0,1
##
.mh.rr.fct <- function(cases, exposure, strata, alpha = 0.05) {
    # ME, Greenland et al, p. 275, 3rd Ed.
    # calculate the individual rr's in the strata
    if (!is.data.frame(strata)) as.data.frame(strata)


    # TODO filter if there is an NA in one of the variables
    ind.filtered <- filter.na(cases, exposure, strata)
    cases <- cases[-ind.filtered]
    exposure <- exposure[-ind.filtered]
    strata <- as.data.frame(strata[-ind.filtered,])

    stratum.defs <- na.omit(unique(strata))

    rr.stratified <- t(apply(stratum.defs,1,
           function(stratum) {

               rr.list <- .rr.fct(table(cases[check.df.equality(stratum,strata)],
                                        exposure[check.df.equality(stratum,strata)]),
                                  alpha = alpha)
               c(rr = rr.list$value,
                 ci.lower = rr.list$conf.int[1],
                 ci.upper = rr.list$conf.int[2])
           }
    ))

    # calculate key quantities for strata
    # TODO change calculations to deal in a consistent way with missing values
    # number of unexposed
    N0 <- apply(stratum.defs,1,
               function(stratum)
                   sum(!exposure[check.df.equality(stratum,strata)],na.rm=T))
    # number of exposed
    N1 <- apply(stratum.defs,1,
                function(stratum)
                    sum(exposure[check.df.equality(stratum,strata)],na.rm=T))
    # number of cases
    M1 <- apply(stratum.defs,1,
                function(stratum)
                    sum(cases[check.df.equality(stratum,strata)],na.rm=T))
    # number of non-cases
    M0 <- apply(stratum.defs,1,
               function(stratum)
                   sum(!cases[check.df.equality(stratum,strata)],na.rm=T))
    # number of cases exposed
    A1 <- apply(stratum.defs,1, function(stratum)
        sum(cases[check.df.equality(stratum,strata)] &
                                       exposure[check.df.equality(stratum,strata)],na.rm=T))

    # number of cases unexposed
    A0 <- apply(stratum.defs,1, function(stratum)
        sum(cases[check.df.equality(stratum,strata)] &
                  !exposure[check.df.equality(stratum,strata)],na.rm=T))
    N.tot <- apply(stratum.defs,1, function(stratum) sum(!is.na(cases[check.df.equality(stratum,strata)]) &
                     !is.na(exposure[check.df.equality(stratum,strata)]),na.rm=T))




    mh.weights <- A0*N1/N.tot
    inv.mh.weights <- A1*N0/N.tot

    individual.rr.table <-
        cbind(stratum.defs, rr.stratified, mh.weights)

    rr.mh <-
        sum(mh.weights/sum(mh.weights)*rr.stratified[,1])

    var.log.rr.mh <- sum((M1*N1*N0 - A1*A0*N.tot)/N.tot^2)/
        (sum(mh.weights)*sum(inv.mh.weights))

    ci.rr.mh.lower <- exp(log(rr.mh) + qnorm(alpha/2)*var.log.rr.mh^(1/2))
    ci.rr.mh.upper <- exp(log(rr.mh) + qnorm(1-alpha/2)*var.log.rr.mh^(1/2))

    rr.crude <- .rr.fct(table(cases,
                              exposure))

    # TODO implement chi square test of homogeneity
    return(
        list(
            rr.by.strat = rr.stratified,
            rr.crude = rr.crude,
            rr.mh = c(RR.mh = rr.mh,
                      CI.lower = ci.rr.mh.lower,
                      CI.upper = ci.rr.mh.upper),
            chi.sq = NA
        )
    )
}
