

ratios = function(data, outcome = "outcome", 
                  exposures = c("exposure1", "exposure2"), 
                  measure = "rr", verbose = F){
 
 
  
  # Function to coerce to class logical and check for binary input
  coercingToNumeric <- function(x){
    # Check for binary input
    if(length(table(x))>2) {stop("More than two groups. Need binary input.")}
    
    # Check for 0 and 1
    if(class(x)=="numeric" & !identical(levels(factor(x)),c("0", "1"))) {stop("Numeric vector must be given as 1 and 0.")}
      

    # Decoding factor or character 
    if (class(x)=="factor"|class(x)=="character") {
      x <- as.character(x)
      x[grepl("y", x, ignore.case=T)] <- "1"
      x[grepl("n", x, ignore.case=T)] <- "0"
      if (!identical(levels(factor(x)),c("0", "1"))) {stop("numierc or logical input needed. Function cannot proceed")}}

      x <- as.numeric(x)
      
  }
  
  
  # input
  out <- data[, which(colnames(data) == outcome)]
  out <- coercingToNumeric(out)
  
  results = NULL
  
  for(i in exposures){
    exp <- data[, which(colnames(data) == i)]
    exp <- coercingToNumeric(exp)
    
    # Construct epitable
    t = epitable(exp, out)
    if(t[1,1]==0) {t[1,1]<-0.5;warning("Fudging used")}
    if(t[1,2]==0) {t[1,2]<-0.5;warning("Fudging used")}
    if(t[2,1]==0) {t[2,1]<-0.5;warning("Fudging used")}
    if(t[2,2]==0) {t[2,2]<-0.5;warning("Fudging used")}
    te = sum(t[2, ]) # total exposed
    ec = t[2, 2] # exposed cases
    ear = ec/te  # exposed attack rate 
    tu = sum(t[1, ]) # total unexposed
    uc = sum(t[1, 2]) # unexposed cases
    uar = uc/tu # unexposed attack rate
    
    # calculate either risk ratios or odds ratios
    if(measure == "rr"){
      y = c(riskratio(t)$measure[2,],
            riskratio(t)$p.value[2,2])
    } else if (measure == "or") {
      y = c(oddsratio(t)$measure[2,],
            oddsratio(t)$p.value[2,2])
    } else { 
      stop("select measure either 'rr' (risk ratio) or 'or' (odds ratio)")
    }
    
    # put results into data.frame
    y = data.frame(i, te, ec, ear, tu, uc, uar, t(y))
    
    # assign column names
    if(measure == "rr"){
      colnames(y) = c("exposure", "exp", "exp.cases", "exp.AR",
                      "unexp", "unexp.cases", "unexp.AR",                 
                      "rr", "lower", "upper", "p.value")
    } else {
      colnames(y) = c("exposure", "exp", "exp.cases", "exp.AR",
                      "unexp", "unexp.cases", "unexp.AR",                 
                      "or", "lower", "upper", "p.value")
    }
    results = rbind(results, y)   
  }
  
  # return either full are restricted results
  if(verbose){
    results
  } else {
    results = results[, c(1, 8:11)]
  }
  
  results
}  


#' calculate risk ratio or odds ratio
#' 
#' Function to create a data.frame of risk ratio or odds ratio results from a 2x2 contingency table for 
#' multiple exposures. Note: this function gives the equivalent of the cs command in STATA. Date: 08.12.2014
#' @param data data.frame containing outcome and exposure data
#' @param outcome  a character vector containing the name of the outcome column of (1s and 0s) in data
#' @param exposure  a character vector containing the names of exposure columns  of (1s and 0s) in data or vectors of exposures (1s and 0s)
#' @param measure a character vector indicating either rr (risk ratio) or 'or' (odds ratio)
#' @param verbose a logical vector, if TRUE gives full results if FALSE gives restricted results
#' @param output a data.frame containing results for each exposure
#' @return A list of results 
#' @author Daniel Gardiner, \email{daniel.gardiner@phe.gov.uk}
#' @keywords epidemiology
#' @export
#' @examples
#' # define dummy data
#' 
#' df = generateOutbreak(dataquality="good", numberExposures = 10, probabilityOfTrueExposue = 0.9)
#' # function examples
#' # function examples
#' ratios(df, outcome = "ill", exposures = c(names(df)[13:dim(df)[2]]))
