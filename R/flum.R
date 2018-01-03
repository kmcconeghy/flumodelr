#' @title flum: General modeling function for Attributable Influenza Mortality
#' 
#' @description General function wrapper to perform a variety of forecasted 
#' model. The output will always be a set of fitted values.
#'
#' @usage flum(data=NULL, model="serflm", outc=NULL, epi=NULL, time=NULL, 
#'               t.interval=52, echo=F)
#'               
#' @param data A dataframe class object, must contain time variable, 
#' epidemic indicator, and measure of influenza morbidity
#' 
#' @param model A character of length 1, indicates which model to fit. Options
#' include "ird" (see ?ird), "serflm" (see ?serflm), virology model (vignette('05-modserf', 
#' package = "flumodelr")), "arima" (see ?flurima).  
#' 
#' @param outc an unquoted name of a column in data which corresponds to 
#' the outcome variable of interest
#' 
#' @param epi an unquoted name of a column in data object (e.g. epi) 
#' or if null will default to Sept - May.  
#' 
#' @param time an unquoted name of a column in data object, must be 
#' a numeric/integer class variable in dataframe which corresponds 
#' to a unit of time, must be unique (i.e. non-repeating)  
#' 
#' @param t.interval a numeric vector indicating period length, i.e. 52 weeks 
#' in year  
#'   
#' @param echo A logical parameter, if T Will output model parameters and 
#' description.   
#'
#' @param ... Individual models may need additional parameters, e.g. for 
#' virology must specify a viral test variable.   
#' 
#' @return an object of class data.frame, input + y0 (fitted values), threshold 
#' if applicable, and computted excess mortality.  
#' 
#' @export
#' 
#' @examples
#' require(flumodelr)
#' fludta <- flumodelr::fludta
#' flu_fit <- flum(fludta, model="serflm", outc = fludeaths, time = yrweek_dt)  
#'               
#' head(flu_fit)
#' 
#' @references 
#' Thompson WW1, Weintraub E, Dhankhar P, Cheng PY, Brammer L, 
#' Meltzer MI, Bresee JS, Shay DK. Estimates of US influenza-associated 
#' deaths made using four different methods. 
#' Influenza Other Respir Viruses. 2009 Jan;3(1):37-49. 
#' /url{https://www.ncbi.nlm.nih.gov/pubmed/19453440}  
#' 
flum <- function(data=NULL, model="serflm", 
                 outc=NULL, epi=NULL, time=NULL, 
                 t.interval=52, echo=F, ...) {
  
  #tidy evaluation  
  outc_eq <- enquo(outc)
  time_eq <- enquo(time)
  epi_eq <- enquo(epi)
  
  #If no epi variable then, generate automatic period from Sept - May. 
  #write epi object as name
  if (epi_eq==quo(NULL)) {
    data <- data %>%
      dplyr::mutate(epi = if_else(month(!!time_eq)>=10 | 
                                    month(!!time_eq)<=5, 
                                  T, F))  
    epi <- "epi"
    epi_eq <- quo(epi)
  } else {epi_eq <- enquo(epi)}
  
  #pull function
    model <- match.fun(model)
  
  #call

  #fit model using call 'model'
  if (is.function(model)) 
    outpt <- model(data=data, 
                              outc=rlang::UQ(outc_eq), 
                              epi=rlang::UQ(epi_eq), 
                              time=rlang::UQ(time_eq), 
                   t.interval=t.interval, echo=echo)
  
  #return result
  return(outpt)
}