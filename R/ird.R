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
#' @param respStart A numeric/integer class variable, must specify the 
#' week number of the start of the respiratory season.
#' The default value is 27. This corresponds with the beginning of July.
#' 
#' @param viral A numeric class variable, must specify the
#' proportion of positive isolates for a week to be considered of
#' "high viral activity" 
#'   
#' @param fluStart A numeric/integer class variable, must specify the
#' week number of the start of the influenza season. The default 
#' value is 40. This corresponds with the beginning of October.
#' 
#' @param fluStop A numeric/integer class variable, must specify the
#' week number of the start of the influenza season. The default 
#' value is 18. This corresponds with the end of April.
#'   
#' @return an object of class data.frame
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
#' Thompson WW, Weintraub E, Dhankhar P, Cheng PY, Brammer L, 
#' Meltzer MI, Bresee JS, Shay DK. Estimates of US influenza‐associated 
#' deaths made using four different methods. Influenza and other 
#' respiratory viruses. 2009 Jan 1;3(1):37-49.
#' /url{http://onlinelibrary.wiley.com/doi/10.1111/j.1750-2659.2009.00073.x/full} 
#' 
ird <- function(data=NULL, 
                flu=NULL, 
                epi=NULL, 
                time=NULL,
                t.interval="wofy"
                respStart=27
                viral=0.1
                fluStart=40
                fluStop=18
                ) {
  cat("incidence rate-difference model \n",
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

    

  #compute rate differences  

  
  #return results

  
}
