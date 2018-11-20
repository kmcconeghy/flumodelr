#' @title fluglm: Generate fitted prediction values with generalized linear model. 
#' 
#' @description Similar to serflm, but allows for more 
#' specifications. 
#'
#' @usage fluglm(data=NULL, outc=NULL, season, viral, 
#'               time=NULL, period=52, echo=F, poly=T, 
#'               model_form='none', int_type="ci", alpha=0.05, offset,
#'               glmnb=F, ...)
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
#' @param glmnb Logical, if True will run a negative binomial model using 
#'              MASS::nb.glm
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
#' flu_fit <- fluglm(fludta, outc = fludeaths, time = yrweek_dt, season=T)  
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
#' @import rlang dplyr
#' 
fluglm <- function(data=NULL, outc=NULL, 
                   season, viral, 
                   time=NULL, period=52, 
                   echo=F, poly=T, model_form='none', 
                   int_type="ci", alpha=0.05, offset, 
                   glmnb=F, ...) {
  #df is data.frame
  stopifnot(is.data.frame(data)) 
  
  #tidy evaluation  
    outc_eq <- enquo(outc)
    time_eq <- enquo(time)
    if ( missing(offset)) {
      offset_eq <- quo(NULL)
    } else {
      offset_eq <- enquo(offset)
    } 

   if ( missing(season)) {
      season_eq <- NULL 
      } else {
        season_eq <- enquo(season)
      } 
  
  # sanity
  if (!missing(season) & !missing(viral)) stop("'season' and 'viral' 
                                               cannot both be specified")
  if (all(missing(season), missing(viral))) stop("neither 'season', 'viral' 
                                                 specified") 
    
  #set-up data
    data <- data %>% dplyr::arrange(UQ(time_eq))
      
    data <- dplyr::mutate(data, 
                    t_unit = row_number(),
                    theta = 2*t_unit/period,
                    sin_f1 = sinpi(theta),
                    cos_f1 = cospi(theta))
  #parameters  
    if (echo==T) {
      cat("Setting regression parameters...\n")
      cat(" 'outc' variable is:", quo_text(outc_eq), "\n")
      if (!missing(season)) {cat(" 'epi' variable is:", quo_text(season_eq), "\n")}
      if (!missing(viral)) {cat(" 'viral' variables:", paste0(viral, sep=", "), "\n")}
      cat(" 'time' variable is:", quo_text(time_eq), "\n")
      cat("  time period is:", period, "\n")
    }
    
  ## Run Model  
    #Build glm formula  
    flu_form <- mk_flu_form(outc=!!outc_eq, 
                            offset=!!offset_eq, 
                            poly=poly,
                            model_form = model_form)
    
    #Seasonal model  
    if (!missing(season)) {
      if (rlang::quo_text(season_eq)=='T') {
      
      data <- mutate(data,
                     epi = if_else(month(!!time_eq)>=10 | month(!!time_eq)<=5, 
                                   T, F))  
      epi <- 'epi'
      season_eq <- quo(epi)
      } 
    
      #SEASONAL MODEL
      if (echo==T) print(paste0("Model formula: ", flu_form))
      
      new_data <- dplyr::filter(data, !!season_eq==F)  
      
      #compute baseline regression 
      argslist <- list(formula=flu_form, 
                       data=new_data,
                       na.action = na.exclude,
                       ...)
      
      base_fit <- if (!glmnb) {
        do.call(glm, args=argslist) 
        } else if (glmnb) {
        do.call(MASS::glm.nb, args=argslist) 
        } 
      
      result <- get_fitvals(base_fit, data)
    }
    
    #VIROLOGY MODEL
    if (!missing(viral)) {
      flu_form <- paste0(flu_form, "+ ", paste(viral, collapse=" + "))
      if (echo==T) { print( paste0("Model formula: ", flu_form))}
      
      ## check viral parameters are canon  
      for (i in viral) {
        if (any(sapply(data[, i], test_prop))==F) {
          warning('variable: ', i, 'exceeds boundary 0 - 1')
        }
      }
      
      #build argument list  
      argslist <- list(formula=flu_form, 
                       data=data,
                       na.action = na.exclude,
                       ...)
      
      #run regression  
      base_fit <- if (!glmnb) {
                    do.call(glm, args=argslist) 
                  } else if (glmnb) {
                              do.call(MASS::glm.nb, args=argslist) 
                    }  
      
      ## Fitted values + prediction interval
      dta_noviral <- data
      dta_noviral[, viral] <- sapply(dta_noviral[, viral], function(x) x=0)
      
      result <- get_fitvals(base_fit, dta_noviral)
    }

  #Report 
  if (echo==T) {
    print(summary(result$base_fit))
  }
  
  data <- tibble::add_column(data, fit = result$fitted, 
                       upper = result$upper.fit,
                       lower = result$lower.fit) %>%
    dplyr::select(-t_unit, -theta, -sin_f1, -cos_f1)
  
  #return results
  return(data)
}



