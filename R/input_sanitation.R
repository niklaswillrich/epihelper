## this file contains helper functions, which check the input
## for conformity with our format


## function to check values of exposure and case
## variables if they only contain 0,1 and NA
## for the cs, cc function

.binary_check <- function(cases, exposure) {
    if (!(
        all(cases == 0 | cases == 1 | is.na(cases)) &
        all(exposure == 0 | exposure == 1 | is.na(exposure))
    )) {
        stop("Case and exposure variable must be encoded 1/0")
    }
}


## function to check if the case var and exposure vars are all string
## variables
.check_all_characters <- function(case.var, exposure.vars) {
    if(!is.character(case.var) | !all(sapply(exposure.vars, is.character))) {
        stop("Case and exposure variables must be strings.")
    }
}


filter.na <- function(...) {
    input <- list(...)

    stopifnot(all(sapply(input,
               function(item) is.data.frame(item) | is.vector(item))))
    input <- lapply(input, function(item) as.data.frame(item))
    stopifnot(all(sapply(input,
                         function(item)
                             dim(item)[1] == dim(input[[1]])[1])))
    data.comb <- Reduce(bind_cols, input, NULL)
    omitted <- na.omit(data.comb)
    return(attr(omitted, "na.action"))
}

