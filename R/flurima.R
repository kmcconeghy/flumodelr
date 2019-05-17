#' @title flurima: ARIMA models applied to attributable mortality
#' 
#' @description This performs similarly to Serfling models, except fit is 
#' done with an ARIMA approach.  
#'
#' @usage flurima()
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
#' @param order A vector of length three, given ARIMA(p,d,q) specifications.  
#' 
#' @param seasonal A vector of length three, given ARIMA(p,d,q) specifications.
#'             
#' @param ... other options passed on to ARIMA model 
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
flurima <- function(data=NULL, outc=NULL, time=NULL, season,
                    order=NULL, seasonal=NULL, 
                    echo=F, period=52, alpha=0.05, ...) {
  
  
  
  #df is data.frame
  stopifnot(is.data.frame(data)) 
  
  #tidy evaluation  
  time_eq <- enquo(time)
  outc_eq <- enquo(outc)
  if ( missing(order)) {
    t.arima = 'auto'
  } else {
    t.arima = 'user'
  } 
  
  
  if ( missing(season)) {
    season_eq <- NULL 
  } else {
    season_eq <- enquo(season)
  } 
  
  if (!(missing(order) & missing(seasonal))) {
    warning('ARIMA order argument specified, but seasonal argument missing, 
            model will not have seasonal component')
  }
  
    data <- dplyr::mutate(data, 
                        t_unit = !!time_eq,
                        theta = 2*t_unit/period,
                        sin_f1 = sinpi(theta),
                        cos_f1 = cospi(theta))
    
    flu_form <- mk_flu_form(outc=!!outc_eq, poly=T)
    
    # Seasonal model  
    if (!missing(season)) {
      if (rlang::quo_text(season_eq)=='T') {
        
        data <- mutate(data,
                       epi = if_else(month(!!time_eq)>=10 | month(!!time_eq)<=5, 
                                     T, F))  
        epi <- 'epi'
        season_eq <- quo(epi)
      }
    }
      
      flu_form <- mk_flu_form(outc=fludeaths, poly=T)
      flu_glmfit <- MASS::glm.nb(flu_form, data=data[data$epi==T, ], na.action = na.exclude)
      
      pred <- predict(flu_glmfit, data)
      
      if (t.arima == 'auto') {
        arima.fit <- auto.arima(pred, stepwise = F, approximation=F, ...)
      }
      if (t.arima == 'user') {
        arima.fit <- auto.arima(pred, order=order)
      }
      
      if (echo) print(arima.fit)
      
      if (echo) checkresiduals(arima.fit)
      
      arima.pred <- arima.fit$fitted
  
    fitted <- arima.pred
    upper.fit <- arima.pred + 
      qnorm(1-alpha, mean=0, sd=1, lower.tail = T, log.p = F) * 
      sd(arima.fit$residuals)
    lower.fit <- upper.fit <- arima.pred - 
      qnorm(1-alpha, mean=0, sd=1, lower.tail = T, log.p = F) * 
      sd(arima.fit$residuals)

    list(fitted = fitted, lower.fit=lower.fit, upper.fit = upper.fit)
  
}