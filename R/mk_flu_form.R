#' @title mk_flu_form: Build model formula for internal use
#' 
#' @description This function is not meant to be called directly, it takes input
#' from user-called functions and builds a formula class object for use in
#' regression commands.  
#'
#' @usage mk_flu_form(outc, offset, poly, model_form)
#'               
#' @return an object of class formula
#' 
#' @export
#' 
#' @import rlang  
#' 
mk_flu_form <- function(outc, offset=NULL, poly=T, model_form='none') {
  outc_eq <- enquo(outc)
  offset_eq <- enquo(offset)

  #build model formula
  flu_form <- paste0(rlang::quo_text(outc_eq), " ~ ", "t_unit", "+ sin_f1", "+ cos_f1")
  
  ## Offset term
  if (!quo_is_null(offset_eq)) flu_form <- paste0(flu_form, "+ offset(", rlang::quo_text(offset_eq), ")")

  ## Add polynomials
  if (poly) flu_form <- paste0(flu_form, "+ I(t_unit^2) + I(t_unit^3) + I(t_unit^4) + I(t_unit^5)")

  ## If user-specified formula  
  if (model_form!='none') {
    return(model_form)
  } else {
    return(flu_form)
  }
}