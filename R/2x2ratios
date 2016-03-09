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
#' set.seed(5)
#' 
#' df = data.frame(outcome = sample(c(0,1), 100, replace = T), 
#'                 exposure1 = sample(c(0,1), 100, replace = T),
#'                 exposure2 = sample(c(0,1), 100, replace = T))
#' # function examples
#' ratios(df, outcome = "outcome", exposures = c("exposure1", "exposure2"))
#' 
#' ratios(df, outcome = "outcome", exposures = c("exposure1", "exposure2"),
#'        measure = "or")
#' # using verbose = TRUE gives same output as you get in stata
#' ratios(df, outcome = "outcome", exposures = "exposure1", verbose = TRUE)
ratios = function(data, outcome = "outcome", 
                  exposures = c("exposure1", "exposure2"), 
                  measure = "rr", verbose = F){

  require(epitools)
  
  out = as.numeric(data[, which(colnames(data) == outcome)])
  
  results = NULL
  
  for(i in exposures){
    exp = as.numeric(data[, which(colnames(data) == i)])
    
    t = epitable(exp, out)
    
    # total exposed
    te = sum(t[2, ])
    
    # exposed cases
    ec = t[2, 2]
    
    # exposed attack rate 
    ear = ec/te
    
    # total unexposed
    tu = sum(t[1, ])
    
    # unexposed cases
    uc = sum(t[1, 2])
    
    # unexposed attack rate
    uar = uc/tu
    
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

