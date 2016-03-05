#' Calculate relative risk
#' 
#' This function computes the relative risk and the two-by-two table
#' @param cases vector of cases
#' @param exposure vectore of exposures
#' @param fudigng If fudging should be used
#' @param alpha alpha value
#' @return A list of results 
#' @keywords epidemiology
#' @export
#' @examples
#' calcRelativeRisk(cases=linelist$ill, linelist$moreno)

################################################################ 
# The relative risk function
################################################################

calcRelativeRisk <- function(cases, exposure, fudging=TRUE, alpha=0.05)
{
  #####################################################
  # Decoding input
  #####################################################
  
  # Initialize warningMessages as a list
  warningMessages <- list() 
  
  # Decoding cases, when cases are factor or character 
  if (class(cases)=="factor"|class(cases)=="character") {
    if(length(levels(factor(cases)))>2) {stop("More than two groups. Need binary input. Function cannot proceed")}
    cases <- as.character(cases)
    cases[grepl("y", cases, ignore.case=T)] <- "1"
    cases[grepl("n", cases, ignore.case=T)] <- "0"
    if (!identical(levels(factor(cases)),c("0", "1"))) {stop("Cases not given as binary. Searching for y and n did not yield digestible results. Function cannot proceed")}
    cases <- as.numeric(cases)
    warningMessages$TypeOfVariableCases <- "Cases not of class binary Binary input preferred."
  }  
  
  # Decoding cases, when cases are numeric
  if(class(cases)=="numeric") {
  if(length(levels(factor(cases)))>2) {stop("More than two groups. Need binary input. Function cannot proceed")}
  if (!identical(levels(factor(cases)),c("0", "1"))) {stop("Cases not given as binary. Searching for 1 and 0 didnt yield digestible results. Function cannot proceed")}
  cases <- as.logical(cases)
  }
  
  # Decoding exposure, when exposure are factor or character 
  if (class(exposure)=="factor"|class(exposure)=="character") {
    if(length(levels(factor(exposure)))>2) {stop("More than two groups. Need binary input. Function cannot proceed")}
    exposure <- as.character(exposure)
    exposure[grepl("y", exposure, ignore.case=T)] <- "1"
    exposure[grepl("n", exposure, ignore.case=T)] <- "0"
    if (!identical(levels(factor(exposure)),c("0", "1"))) {stop("exposure not given as binary. Searching for y and n didnt yield digestible results. Function cannot proceed")}
    exposure <- as.numeric(exposure)
    warningMessages$TypeOfVariableExposure <- "Exposure not as class binary. Binary input preferred."
  }
  
  # Decoding exposures, when exposure are numeric
  if(class(exposure)=="numeric") {
    if(length(levels(factor(exposure)))>2) {stop("More than two groups. Need binary input. Function cannot proceed")}
    if (!identical(levels(factor(exposure)),c("0", "1"))) {stop("Cases not given as binary. Searching for 1 and 0 didnt yield digestible results. Function cannot proceed")}
    exposure <- as.logical(exposure)
  }
  
  ###################################################
  # Simple relative risk 
  ###################################################
  
  
  # Calculating a,b,c,d with probability weight
  a=sum((cases)[exposure], na.rm=T) #CaseExposed
  b=sum((cases)[!exposure], na.rm=T) #CaseUnexposed
  c=sum((!cases)[exposure], na.rm=T) #NocaseExposed
  d=sum((!cases)[!exposure], na.rm=T) #NocaseUnexposed
  
  # Fudging
  if(a==0|b==0|c==0|d==0) {print("Fudging used");if(a==0) a=0.5;if(b==0) b=0.5;if(c==0) c=0.5;if(d==0) d=0.5}
  
  # Generate two by two table
  twoByTwoTable <- matrix(c(a,b,c,d), ncol=2)
  twoByTwoTable <- rbind(twoByTwoTable, margin.table(twoByTwoTable, 2))
  twoByTwoTable <- cbind(twoByTwoTable, margin.table(twoByTwoTable, 1))
  colnames(twoByTwoTable) <- c("Cases", "No Cases", "Total")
  rownames(twoByTwoTable) <- c("Exposed", "Unexposed", "Total")
  simpleTwoByTwoTable <- round(twoByTwoTable,1)
  
  # Calculating relative risk
  PExposed=a/(a+c)
  PUnexposed=b/(b+d)
  simpleRelativeRisk = PExposed/PUnexposed
  
  # Calculate a confidence interval
  confidenceLevel <- (1 - alpha)*100
  sigma <- sqrt((1/a) - (1/(a+c)) + (1/b) - (1/(b+d)))
  z <- qnorm(1-(alpha/2))
  simpleLowervalue <- simpleRelativeRisk * exp(-z * sigma)
  simpleUppervalue <- simpleRelativeRisk * exp( z * sigma)
  
  
  
  ###################################################
  # Storing result
  ###################################################
  FinalList <- list(relativRisk = list(simple=simpleRelativeRisk), 
                    confidenceInteval=list(simpleCI=c(simpleLowervalue, simpleUppervalue)), 
                    twoByTwoTable=list(simple=simpleTwoByTwoTable))
  
  ###################################################
  # Hand over results
  ###################################################
  FinalList
  
}  



