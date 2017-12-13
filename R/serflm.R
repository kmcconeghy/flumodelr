#' @title serflm
#' 
#' @description Performs a cyclical linear regression model, 
#' or 'serfling' model
#'
#' @param data A dataframe class object, must contain time variable, 
#' epidemic indicator, and measure of influenza morbidity
#' 
#' @param outc A character string of length=1, identifies 
#' a numeric type variable in dataframe data which is the measure 
#' of disease morbidity / mortality
#' 
#' @param epi either a character string of length=1 (e.g. '"epi")', 
#' identifying a logical type variable in dataframe data which indicates epidemic
#' vs non-epidemic periods, or a vector of length=2 which identifies 
#' periods of epidemicity, e.g. 'c(40,20)'.  
#' 
#' @param time A character string of length=1, identifies 
#' a numeric/integer class variable in dataframe which corresponds 
#' to a unit of time, must be unique (i.e. non-repeating)  
#' 
#' @param t.interval A character string, must specify whether 
#' unit of time cycle is weeks/years ("wofy", i.e. 52 weeks), 
#' or month/years ("mofy"), days/years("dofy")  
#'   
#' @return an object of class data.frame (input dataframe + fitted values)
#' 
#' @export
#' 
#' @examples
#' require(flumodelr)
#' flu_ex <- flumodelr::flu_ex
#' fit <- serflm(data=flu_ex, outc = "fludeaths", epi="epi", time="yrweek_dt")
#'               
#' summary(fit)
#' 
#' @references 
#' Serfling RE. Methods for current statistical analysis of 
#' excess pneumonia-influenza deaths. Public Health Rep. 1963 Jun; 
#' 78(6): 494 - 506.  
#' /url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1915276/} 
#' 
serflm <- function(data=NULL, outc=NULL, epi=NULL, time=NULL, 
                   t.interval="wofy") {
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
      if (is.na(outc)==T) {
        stop("Outcome variable not specified, must specify, see ?serflm")
      }
      if (outc %in% names(data)==F) {
        stop("Outcome variable not named in data object, see ?serflm")
      }
      if (is.numeric(data[[outc]])==F) {
        stop("Outcome variable not type numeric, see ?serflm")
      }
    
    #epidemic period variable exists and valid  
    try(if(is.na(epi)) stop("epi variable not specified, must specify epidemic periods, see ?serflm"))
    
    if (epi %in% names(data)==T) {
      try(if (is.logical(data[[epi]])==F) stop("epi variable not type logical, must be T/F or 0/1, see ?serflm"))
      } else if (length(epi)==2) {
        try(if(class(time)==class(epi)) stop("time variable and epi vector class do not match"))
      }       

    #time variable
      try(if (is.na(time)==T) stop("time variable not specified, must specify, see ?serflm"))
      try(if (time %in% names(data)==F) stop("time variable not named in data object, see ?serflm"))
    
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
    
      data <- data %>%
        mutate(t_unit = row_number(),
               theta = 2*t_unit/t_interval,
               sin_f1 = sinpi(theta),
               cos_f1 = cospi(theta))

    #build model formula
    flu.form <- as.formula(
      paste0(as.name(outc), " ~ ", "t_unit", "+ sin_f1", "+ cos_f1"))

    cat("Model Formula: \n")
    print(flu.form, showEnv=F)
    cat("t_unit is: ", time, "\n", "sin_f1 / cos_f1 is a Fourier term")
    
  #compute baseline regression  
  nepi <- as.name(epi)  
  base_fit <- data %>%
      filter(UQ(as.name(epi))==F) %>%
      lm(flu.form, data=., na.action = na.exclude)
  
  ## Fitted values + prediction interval
  df_pred <- data %>%
    predict(base_fit, newdata=., se.fit=TRUE, 
            interval="prediction", level=0.95)
  
  pred_y0 <- df_pred$fit[,1] #fitted values
  pred_y0_serf <- df_pred$fit[,1] + 1.64*sd(df_pred$fit[,1]) 
  
  data <- data %>%
    add_column(., pred_y0, pred_y0_serf) %>%
    select(-t_unit, -theta, -sin_f1, -cos_f1)
  
  #return results
  return(data)
  
}