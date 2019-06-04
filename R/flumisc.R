#' @title test_prop: Evaluate if numeric vector is a proportion
#' 
#' @usage test_prop(x)
#' 
#' @param x A numeric vector  
#' 
#' @export  
#' 
test_prop <- function(x) {
  stopifnot(is.numeric(x))
  return(dplyr::between(x, 0, 1))
}

#' @title epiweek_dt: Convert an CDC week number to a calendar date
#' 
#' @usage epiweek_dt(year, week)
#' 
#' @param year A numeric vector  
#' @param week A numeric vector  
#' 
#' @export  
#' 
epiweek_dt <- function(year, weeknum) {
  
  #Compute 4th day in January
  jan4 <- ymd(paste(year, 1, 4, sep="-"))
  
  #Compute day of week
  DofW <- wday(jan4, week_start = 7)-1 #Sunday start
  #The -1 is  correction for how wday() counts days  
  
  #If Jan 4th was Sun, starting week date is 01/04/xxxx
  startweek <- if_else(DofW == 7, jan4, (jan4 - (DofW)))
  
  #First Date of Week
  d0 = startweek + (weeknum - 1) * 7
  
  #Last Date of Week
  d1 = startweek + (weeknum - 1) * 7 + 6 
  
  return(list(d0 = ymd(d0), d1 = ymd(d1)))
}