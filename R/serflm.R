#' @title serflm
#' 
#' @description Performs a cyclical linear regression model, 
#' or 'serfling' model
#'
#' @param data A dataframe class object
#' 
#' @return an object of class lm.summary
#' 
#' @export
#' 
#' @examples
#' require(flumodelr)
#' cdc122 <- flumodelr::cdc122city
#' fit <- serflm(data=cdc122)
#' summary(fit)
#' 
#' @references 
#' Serfling RE. Methods for current statistical analysis of 
#' excess pneumonia-influenza deaths. Public Health Rep. 1963 Jun; 
#' 78(6): 494 - 506.  
#' /url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1915276/} 
#' 
serflm <- function(data=NULL) {
  
  #sanity checks
  
  #return results
  return(2+2)
  
}