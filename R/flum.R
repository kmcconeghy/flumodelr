#' @title flum: General modeling function for Attributable Influenza Mortality
#' 
#' @description General function wrapper to perform a variety of forecasted 
#' models. The output will always be a set of fitted values.
#'
#' @usage flum(data=NULL, model=NULL, ...)
#'               
#' @param data A dataframe class object, must contain time variable, 
#' epidemic indicator, and measure of influenza morbidity
#' 
#' @param model A character of length 1, indicates which model to fit. Options
#' include "ird" (see ?ird), "serflm" (see ?serflm), "fluglm", "flunb", "arima" (see ?flurima).  
#' 
#' @param ... Extra arguments depending on model requested, see examples and vignettes.
#' 
#' @return an object of class data.frame, input + y0 (fitted values), threshold 
#' if applicable, and computed excess morbidity  
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
#' @import rlang 
#' 
flum <- function(data=NULL, model=NULL, ...) {
  
  #pull function
    model <- match.fun(model)
  
  #call

  #fit model using call 'model'
  if (is.function(model)) 
    outpt <- model(data=data, ...)
  
  #return result
  return(outpt)
}