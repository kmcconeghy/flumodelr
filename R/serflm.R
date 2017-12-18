#' @title serflm: Fit a Serling Model on Time Series Data
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
#' vs non-epidemic periods, or if unspecified will default to Sept - May.  
#' 
#' @param time A character string of length=1, identifies 
#' a numeric/integer class variable in dataframe which corresponds 
#' to a unit of time, must be unique (i.e. non-repeating)  
#' 
#' @param t.interval A character string, must specify whether 
#' unit of time cycle is weeks/years ("wofy", i.e. 52 weeks), 
#' or month/years ("mofy"), days/years("dofy")  
#'   
#' @return an object of class data.frame, input + y0 (fitted values), y0_ul 
#' the upper serfling threshold
#' 
#' @export
#' 
#' @examples
#' require(flumodelr)
#' flu_ex <- flumodelr::flu_ex
#' flu_fit <- serflm(flu_ex, outc = "fludeaths", time = "yrweek_dt")  
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
      stopifnot(is.data.frame(data)) 
      
    #flu variable exists and valid  
      stopifnot (is.na(outc)==F, (outc %in% names(data)==T),
                 (is.numeric(data[[outc]])==T)) 
    
    #If no epi variable then, generate automatic period from Sept - May. 
      #write epi object as name
      if (is.null(epi)) {
        data <- data %>%
          mutate(epi = if_else(month(UQ(as.name(time)))>=10 | 
                                       month(UQ(as.name(time)))<=5, 
                                             T, F))  
        epi <- "epi"
      }
      stopifnot(epi %in% names(data))
      try(if (is.logical(data[[epi]])==F) stop("epi variable not type logical, must be T/F or 0/1, see ?serflm"))

    #time variable
      try(if (is.na(time)==T) stop("time variable not specified, must specify, see ?serflm"))
      try(if (time %in% names(data)==F) stop("time variable not named in data object, see ?serflm"))
    
      stopifnot(n_distinct(time)==length(time),
                (t.interval %in% names(t.intervals)==T))
      
  data <- data %>% arrange(., UQ(as.name(time)))
    
  #parameters  
  cat("Setting regression parameters...\n")
    cat(" 'epi' variable is:", epi, "\n")
    cat(" 'time' variable is:", time, "\n")
    
    t_interval <- t.intervals[t.interval]
    cat("  time period is:", t_interval, "\n")
    
      data <- data %>%
        mutate(t_unit = row_number(),
               theta = 2*t_unit/t_interval,
               sin_f1 = sinpi(theta),
               cos_f1 = cospi(theta))

    #build model formula
    flu.form <- as.formula(
      paste0(as.name(outc), " ~ ", "t_unit", "+ sin_f1", "+ cos_f1"))

  #compute baseline regression  
  base_fit <- data %>%
      filter(UQ(as.name(epi))==F) %>%
      lm(flu.form, data=., na.action = na.exclude)
  
  ## Fitted values + prediction interval
  y0 <- data %>%
    predict(base_fit, newdata=.)
  
  y0_ul <- y0 + 1.64*sd(y0) 
  
  data <- data %>%
    add_column(., y0, y0_ul) %>%
    select(-t_unit, -theta, -sin_f1, -cos_f1)
  
  #return results
  return(data)
  
}