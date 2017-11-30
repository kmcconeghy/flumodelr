#' @title serflm
#' 
#' @description Performs a cyclical linear regression model, 
#' or 'serfling' model
#'
#' @param 
#' @return an object of class lm.summary
#' @export
#' @examples
#' require(flumodelr)
#' cdc122 <- flumodelr::cdc122
#' m.out <- serflm(treat ~ age + educ + black + hispan, 
#'                 data=cdc122)
#' summary(m.out)
#'
#'

serflm <- function(data=NULL) {
  #sanity checks
  
  #return results
  return(2+2)
}