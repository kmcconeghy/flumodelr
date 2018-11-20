#' @title flunb: Generate fitted prediction values with negative binomial
#' model. 
#' 
#' @description Similar to fluglm, but glm does not have a negative
#' binomial option so this function calls MASS::glm.nb separately.
#'
#' @usage flunb(data=NULL, outc=NULL, season, viral, time=NULL, 
#'               period=52, echo=F, poly=T, model_form='none',
#'               int_type="ci", alpha=0.05, offset=NULL, ...)
#'               
#' @param data A dataframe class object, must contain time variable, 
#' epidemic indicator, and measure of influenza morbidity
#' 
#' @param outc an unquoted name of a column in data which corresponds to 
#' the outcome variable of interest
#' 
#' @param season Either 'T' (where the epidemic baseline model will be done based on month of year),
#'  or the name of a column which is a logical vector flagging a given week as epidemic or not 
#' 
#' @param viral a string vector naming 1 or more viral specimens   
#' 
#' @param time an unquoted name of a column in data object, must be 
#' a numeric/integer class variable in dataframe which corresponds 
#' to a unit of time, must be unique (i.e. non-repeating)  
#' 
#' @param period a numeric vector indicating period length, i.e. 52 weeks in year  
#'   
#' @param echo A logical, if T will print variables used in model.  
#' 
#' @param poly A logical, if T will include a quadratic, cubic and 
#' quartic term.  
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
#' @param offset Specify if offset term to be used, must specify log(object)  
#' 
#' @param ... other options passed on to glm model (e.g. family=poisson, see ?glm)  
#'   
#' @return an object of class data.frame, fit, upper and lower confidence bounds
#' 
#' @export
#' 
#' @examples
#' require(flumodelr)
#' fludta <- flumodelr::fludta
#' flu_fit <- flunb(fludta, outc = fludeaths, time = yrweek_dt)  
#'               
#' head(flu_fit)
#' 
#' @references 
#' Thompson WW1, Weintraub E, Dhankhar P, Cheng PY, Brammer L, 
#' Meltzer MI, Bresee JS, Shay DK. Estimates of US influenza-associated 
#' deaths made using four different methods. Influenza Other Respir 
#' Viruses. 2009 Jan;3(1):37-49. 
#' /url{https://www.ncbi.nlm.nih.gov/pubmed/19453440}
#' 
#' @import rlang  
#' 
flunb <- function(data=NULL, outc=NULL, 
                   season, viral, 
                   time=NULL, period=52, 
                   echo=F, poly=T, model_form='none', 
                   int_type="ci", alpha=0.05, offset=NULL, ...) {
  #df is data.frame
  stopifnot(is.data.frame(data)) 
  
  #tidy evaluation  
  outc_eq <- enquo(outc)
  time_eq <- enquo(time)
  offset_eq <- enquo(offset)  
  
  if (missing(season)) {
    season_eq <- NULL 
    } else {
    season_eq <- enquo(season)
    } 
  
  if (!missing(season) & !missing(viral)) stop("'season' and 'viral' 
                                               cannot both be specified")
  
  #set-up data
  data <- data %>% dplyr::arrange(., !!time_eq)
  
  data <- data %>%
    dplyr::mutate(t_unit = row_number(),
                  theta = 2*t_unit/period,
                  sin_f1 = sinpi(theta),
                  cos_f1 = cospi(theta))
  #parameters  
  if (echo==T) {
    cat("Setting regression parameters...\n")
    cat(" 'outc' variable is:", rlang::quo_text(outc_eq), "\n")
    if (!missing(season)) {cat(" 'epi' variable is:", rlang::quo_text(season_eq), "\n")}
    if (!missing(viral)) {cat(" 'viral' variables:", paste0(viral, sep=", "), "\n")}
    cat(" 'time' variable is:", rlang::quo_text(time_eq), "\n")
    cat("  time period is:", period, "\n")
  }
  
  ## Run Model  
  flu_form <- mk_flu_form(outc=outc, 
                          offset=offset, 
                          poly=poly, 
                          model_form = model_form)
  
  if (!missing(season)) result <- flunbglm.season()
  if (!missing(viral)) result <- flunbglm.viral()
  
  data <- data %>%
    tibble::add_column(., fit = result$fitted, 
                       upper = result$upper.fit,
                       lower = result$lower.fit) %>%
    dplyr::select(-t_unit, -theta, -sin_f1, -cos_f1)
  
  #return results
  return(data)
}