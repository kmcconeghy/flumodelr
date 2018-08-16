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