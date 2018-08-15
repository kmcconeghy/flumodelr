#' @title fludiff: Compute excess mortality
#' 
#' @description This function takes fitted data from a serflm() or similar 
#' procedure and computes difference between observed and predicted mortality.    
#' 
#' @param data A dataframe class object, must contain observed, fitted variables
#' 
#' @param obsvar named observed outcome variable  
#' 
#' @param fitvar named fitted trend variable. Can specify either fit line, or 
#' upper threshold limit.  
#' 
#' @param serfrule Logical, default=F. If T the function will apply the 
#' Serfling rule which only counts consecutive observations of excess 
#' mortality. 
#' 
#' @return an object of class dataframe  
#' 
#' @export
#' 
#' @examples
#' require(flumodelr)
#' df <- flumodelr::fludta
#' serf_fit <- fluglm(fludta, outc = fludeaths , time = yrweek_dt,
#' offset = log(alldeaths), )  
#'               
#' excess_fludeaths <- fludiff(flu_fit, xvar=yrweek_dt, yvar=fludeaths)
#' 
#' @import rlang dplyr magrittr

fludiff <- function(data=NULL, obsvar=NULL, fitvar=NULL, def="fit", serfrule=F) {
  
  #tidy evaluation  
  obs_eq <- enquo(obsvar)
  fit_eq <- enquo(fitvar)
  
  if (serfrule==F) {
    data <- mutate(data, y_diff = if_else(((!!obs_eq) - (!!fit_eq))>0,
                                            (!!obs_eq) - (!!fit_eq), 0)
    )
  } else if (serfrule==T) {
    
  }
  
  #return dataframe
  return(data)
}