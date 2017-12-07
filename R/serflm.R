#' @title serflm
#' 
#' @description Performs a cyclical linear regression model, 
#' or 'serfling' model
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
#' @param smooth A logical type indicator, if set T, then model will
#' include a set of polynomial terms (time-unit*time-unit) which 
#' will smooth the fitted line.  
#'   
#' @return an object of class lm.summary
#' 
#' @export
#' 
#' @examples
#' require(flumodelr)
#' flu_ex <- flumodelr::flu_ex
#' fit <- serflm(data=flu_ex, 
#'               flu = "fludeaths", epi="epi", 
#'               time="yrweek_dt", smooth=T)
#'               
#' summary(fit)
#' 
#' @references 
#' Serfling RE. Methods for current statistical analysis of 
#' excess pneumonia-influenza deaths. Public Health Rep. 1963 Jun; 
#' 78(6): 494 - 506.  
#' /url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1915276/} 
#' 
serflm <- function(data=NULL, flu=NULL, epi=NULL, time=NULL, 
                   t.interval="wofy", smooth=F) {
  cat("Cyclical Regression Model \n",
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
    
    if (smooth==F) { 
      data <- data %>%
        mutate(t_unit = row_number(),
               theta = 2*t_unit/t_interval,
               sin_f1 = sinpi(theta),
               cos_f1 = cospi(theta))
    } else if (smooth==T) { # Add polynomial functions
      data <- data %>%
        mutate(t_unit = row_number(),
               theta = 2*t_unit/t_interval,
               sin_f1 = sinpi(theta),
               cos_f1 = cospi(theta),
               t_unit2 = t_unit^2,
               t_unit3 = t_unit^3,
               t_unit4 = t_unit^4,
               t_unit5 = t_unit^5)
    }

    #build model formula
    if (smooth==F) { 
    flu.form <- as.formula(
      paste0(as.name(flu), " ~ ", "t_unit", "+ sin_f1", "+ cos_f1"))
    } else if (smooth==T) { 
      flu.form <- as.formula(
        paste0(as.name(flu), " ~ ", "t_unit", "+ t_unit2", "+ t_unit3",
               "+ t_unit4", "+ t_unit5", "+ sin_f1", "+ cos_f1"))
    }
    
    cat("Model Formula: \n")
    print(flu.form, showEnv=F)
    cat("t_unit is: ", time, "\n 
        suffix [0-9] represents exponent (i.e. t_unit2 = t_unit^2)", "\n 
        sin_f1 / cos_f1 is a Fourier term")
    
  #compute regression  
   fit <- lm(flu.form, data=data, na.action = na.exclude)
  
  #return results
  return(fit)
  
}