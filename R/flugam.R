#' @title flugam: compute influenza estimates using Generalized Additive Models 
#' from the mgcv package.
#' 
#' @description This functions take an alternative approach to the trigonometric
#' calculations and using spline functions and generalized additive models 
#' to fit a seasonal influenza curve.  
#'
#' @usage flugam(data=NULL, outc=NULL, season, viral, 
#'               time=NULL, echo=F, model_form='none', 
#'               int_type="ci", alpha=0.05, ...)
#'               
#' @param data A dataframe class object, must contain time variable, 
#' epidemic indicator, and measure of influenza morbidity
#' 
#' @param outc an unquoted name of a column in data which corresponds to 
#' the outcome variable of interest
#' 
#' @param season The name of a column which is a logical vector flagging a 
#' given time point as epidemic or not
#' 
#' @param viral a string vector naming 1 or more viral specimens   
#' 
#' @param time an name of a column in data object, must be 
#' a numeric/integer class, must be unique (i.e. non-repeating)  
#'  
#' @param echo A logical, if T will print variables used in model.  
#' 
#' @param model_form An object of type formula, allowing for user-specified 
#' model to be passed on to glm(). Default missing.    
#' 
#' @param int_type Specifies type of upper interval to be output, currently
#' only allows for confidence intervals. Prediction intervals to be added,  
#' but are only approximate for Poisson families.  
#' 
#' @param alpha The threshold for CI interval, default is 0.05 (one-sided).  
#'              
#' @param ... other options passed on to gam model
#'   
#' @return an object of class data.frame, fit, upper and lower confidence bounds
#' 
#' @export
#' 
#' @examples
#' 
#' @references 
#' Gul D, Cohen C, Tempia S, Newall AT, Muscatello DJ. Influenza-associated 
#' mortality in South Africa, 2009-2013: The importance of choices related to
#' influenza infection proxies. Infl Oth Resp Vir 2017, 12 (1): 54-64.
#' /url{https://www.ncbi.nlm.nih.gov/pubmed/29197161}
#' 
#' @import rlang dplyr mgcv
#' 
flugam <- function(data=NULL, outc=NULL, 
                   season, viral, time=NULL,  
                   echo=F, model_form='none', 
                   int_type="ci", alpha=0.05,  
                   ...) {
  #df is data.frame
  stopifnot(is.data.frame(data)) 
  
  #tidy evaluation  
  outc_eq <- enquo(outc)
  time_eq <- enquo(time)
  
  if (!missing(season)) season_eq <- enquo(season)

  # sanity
  if (!missing(season) & !missing(viral)) stop("'season' and 'viral' 
                                               cannot both be specified")
  if (all(missing(season), missing(viral))) stop("neither 'season', 'viral' 
                                                 specified") 
  
  #set-up data  
  ## prepare time_var
  data <- data %>% dplyr::arrange(!!time_eq)
  
  #parameters  
  if (echo==T) {
    cat("Setting regression parameters...\n")
    cat(" 'outc' variable is:", quo_text(outc_eq), "\n")
    if (!missing(season)) {cat(" 'epi' variable is:", quo_text(season_eq), "\n")}
    if (!missing(viral)) {cat(" 'viral' variables:", paste0(viral, sep=", "), "\n")}
    cat(" 'time' variable is:", quo_text(time_eq), "\n")
  }
  
  ## Run Model  
  #Build gam formula  
  #SEASONAL MODEL
  flu_form <- paste0('fludeaths ', 
                                '~ ', 
                                's(' , quo_name(time_eq), ', bs="ps"', ')')
  
    if (!missing(season)) {
      
      if (echo==T) { 
        cat("Model formula: ") 
        print(flu_form)
      }
      
      # Full Model fitted values  
      base_fit1 <- gam(as.formula(flu_form), data=data, family=nb(), ...)
      
      #fit
      result_1  <- gam_fitvals(base_fit1, data)
      
      #compute baseline regression 
      new_data <- dplyr::filter(data, !!season_eq==F) %>% as.data.frame(.) 
  
      base_fit0 <- gam(as.formula(flu_form), data=new_data, ...)
    
      #fit baseline
      result_0 <- gam_fitvals(base_fit0, data)
    }

  #Viral model  
  if (!missing(viral)) {
   
    flu_form <- paste0(flu_form, "+ ", paste('s(', 
                                             viral, 
                                             ',bs="ps"',
                                             ')', 
                                             collapse=" + "))
    if (echo==T) { 
      cat("Model formula: ") 
      print(as.formula(flu_form))
    }
    
    # Full Model fitted values  
    base_fit1 <- gam(as.formula(flu_form), data=data, family=nb(), ...)
  
    ## Fitted values + prediction interval
    dta_noviral <- data
    dta_noviral[, viral] <- sapply(data[, viral], function(x) x=0)
    
    #fit baseline
    result_0 <- glm_fitvals(base_fit1, dta_noviral)
    
    #fit
    result_1 <- glm_fitvals(base_fit1, data)
  }
  #Report 
  if (echo==T) {
    print(summary(base_fit1))
  }
  
  fit <- bind_cols(flu_pred     = result_1$fitted, 
                   flu_pred_upr = result_1$upper.fit,
                   flu_pred_lwr = result_1$lower.fit,
                   flu_base     = result_0$fitted, 
                   flu_base_upr = result_0$upper.fit,
                   flu_base_lwr = result_0$lower.fit)
  
  #return results
  return(fit)
}