#' @title flunnet: Estimating attributable mortality with Neural Networks
#' 
#' @description Estimates fitted curves and baseline using the `nnetar` function
#' in the forecast package. This is a Feed-forward neural network 
#' with a single hidden layer and lagged inputs for forecasting 
#' univariate time series.
#'
#' @usage flunnet()
#'               
#' @param data A dataframe class object, must contain time variable, 
#' epidemic indicator, and measure of influenza morbidity
#' 
#' @param outc an unquoted name of a column in data which corresponds to 
#' the outcome variable of interest
#' 
#' @param season Either 'T', where the epidemic baseline model will be created 
#' assuming time variable is a date, and Oct-May is epidemic season, or the 
#' name of a column which is a logical vector flagging a given week as 
#' epidemic or not
#' 
#' @param time an unquoted name of a column in data object, must be 
#' a numeric/integer class variable in dataframe which corresponds 
#' to a unit of time, must be unique (i.e. non-repeating)  
#' 
#' @param echo A logical, if T will print variables used in model.  
#' 
#' @param ... other options passed on to `nnetar()`
#'   
#' @return an object of class data.frame, fit, upper and lower confidence bounds
#' 
#' @export
#' 
#' @examples
#' 
#' @references 
#' 
#' @import rlang dplyr forecast
#' 
flunnet <- function(data=NULL, outc=NULL, time=NULL, season,
                    group, echo=F, period=52, alpha=0.05, ...) {
  
  
  
  #df is data.frame
  stopifnot(is.data.frame(data)) 
  
  #tidy evaluation  
  time_eq <- enquo(time)
  outc_eq <- enquo(outc)
  group_eq <- enquo(group) 
  
  if ( missing(season)) {
    season_eq <- NULL 
  } else {
    season_eq <- enquo(season)
  } 
    
  #SEASONAL MODEL
    if (!missing(season)) {
    
    # compute baseline regression 
    base_data <- dplyr::filter(data, !!season_eq==F) %>%
                 flubase(., !!outc_eq, !!time_eq, !!group_eq)
    
    base_fit0 <- nnetar(y = pull(base_data, !!outc_eq))
    result_0 <- forecast(base_fit0, h=34, PI=T, level=0.95)
    
    base_fit1 <- nnetar(data[, quo_name(outc_eq)])
    
    
    
    # Full Model fitted values 
    result_1  <- forecast(base_fit1, PI=T, )
    
    
    #fit baseline
  }
      
      
  
  
}