#' @title ird: Perform incidence rate-difference models
#' 
#' @description Calculates the rate difference between influenza season 
#' and peri-influenza season, or between influenza season and
#' summer season
#' 
#' @param data A dataframe class object, must contain time variable, 
#' epidemic indicator, and measure of influenza morbidity
#' 
#' @param flu A character string of length=1, identifies 
#' a numeric type variable in dataframe data which is the measure 
#' of disease morbidity / mortality
#' 
#' @param epi A character string of length=1, identifies 
#' a logical type variable in dataframe data which indicates epidemic
#' vs non-epidemic periods  
#' 
#' @param time A character string of length=1, identifies 
#' a numeric/integer class variable in dataframe which corresponds 
#' to a unit of time, must be unique (i.e. non-repeating)  
#' 
#' @param t.interval A character string, must specify whether 
#' unit of time cycle is weeks/years ("wofy", i.e. 52 weeks), 
#' or month/years ("mofy"), days/years("dofy")  
#' 
#' @param t.respStart A character string, must specify whether 
#' unit of time cycle is weeks/years ("wofy", i.e. 52 weeks), 
#' or month/years ("mofy"), days/years("dofy")  
#'   
#' @param t.respStop A character string, must specify whether 
#' unit of time cycle is weeks/years ("wofy", i.e. 52 weeks), 
#' or month/years ("mofy"), days/years("dofy")  
#'   
#' @return 
#' 
#' @export
#' 
#' @examples
#' require(flumodelr)
#' flu_ex <- flumodelr::flu_ex
#' fit <- serflm(data=flu_ex, 
#'               flu = "fludeaths", epi="epi", 
#'               time="yrweek_dt")
#'               
#' summary(fit)
#' 
#' @references 
#' Serfling RE. Methods for current statistical analysis of 
#' excess pneumonia-influenza deaths. Public Health Rep. 1963 Jun; 
#' 78(6): 494 - 506.  
#' /url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1915276/} 
#' 
ird <- function(data=NULL, flu=NULL, epi=NULL, time=NULL, 
                   t.interval="wofy") {
  cat("Periseasonal Adjustment Model \n",
      paste0(rep("=", 60), collapse=""), "\n")
  
  #internal objects  
    t.intervals = c("wofy"=52, "mofy"=12, "dofy"=365)
      
  #sanity checks
    #df is data.frame
      if (is.data.frame(data)==F) {
        stop("Data object is not a data.frame class object")
      }
    #flu variable exists and valid  
      if (is.na(flu)==T) {
        stop("influenza variable not specified, must specify, see ?serflm")
      }
      if (flu %in% names(data)==F) {
        stop("flu variable not named in data object, see ?serflm")
      }
      if (is.numeric(data[[flu]])==F) {
        stop("flu variable not type numeric, see ?serflm")
      }
    
    #epidemic period variable exists and valid  
      if (is.na(epi)==T) {
        stop("epi variable not specified, must specify epidemic periods, see ?serflm")
      }
      if (epi %in% names(data)==F) {
        stop("epi variable not named in data object, see ?serflm")
      }
      if (is.logical(data[[epi]])==F) {
        stop("epi variable not type logical, must be T/F or 0/1, see ?serflm")
      }
  
    #time variable
      if (is.na(time)==T) {
        stop("time variable not specified, must specify, see ?serflm")
      }
      if (time %in% names(data)==F) {
        stop("time variable not named in data object, see ?serflm")
      }
      if (n_distinct(time)!= length(time)) {
        stop("time variable repeats, should be unique, see ?serflm")
      }
      if (t.interval %in% names(t.intervals)==F) {
        stop("time interval not a valid option, see ?serflm")
      }
  ntime <- as.name(time)
  data <- data %>% arrange(., !!ntime)
    
  #parameters  
  cat("Setting regression parameters...\n")
    cat(" 'epi' variable is:", epi, "\n")
    cat(" 'time' variable is:", time, "\n")
    
    t_interval <- t.intervals[t.interval]
    cat("  time interval is:", t_interval, "\n")
    
 
    #build model formula

    

  #compute rate with peri-seasonal adjustment  

  
  #return results

  
}
