#' @title flunb: Generate fitted prediction values with negative binomial
#' model. 
#' 
#' @description Similar to fluglm, but glm does not have a negative
#' binomial option so this function calls MASS::glm.nb separately.
#'
#' @usage flunb(data=NULL, outc=NULL, time=NULL, 
#'               period=52, echo=F, poly=T, model.form=NULL,
#'               season = NULL, viral=NULL, 
#'               int_type="ci", alpha=0.1)
#'               
#' @param data A dataframe class object, must contain time variable, 
#' epidemic indicator, and measure of influenza morbidity
#' 
#' @param outc an unquoted name of a column in data which corresponds to 
#' the outcome variable of interest
#' 
#' @param season  
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
#' @return an object of class data.frame, input + y0 (fitted values), y0_ul 
#' the upper threshold
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
#' @import rlang dplyr magrittr MASS  
#' 
flunb <- function(data=NULL, outc=NULL, 
                   season, viral, 
                   time=NULL, period=52, 
                   echo=F, poly=T, model_form=NULL, 
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
  data <- data %>% dplyr::arrange(., UQ(time_eq))
  
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
  
  fluglm.model <- function() {
    #build model formula
    flu_form <- paste0(rlang::quo_text(outc_eq), 
                       " ~ ", "t_unit", "+ sin_f1", "+ cos_f1")
    ## Offset term
    if (offset_eq!=quo(NULL)) {
      flu_form <- paste0(flu_form, 
                         "+ offset(", rlang::quo_text(offset_eq), ")")
    }
    
    ## Add polynomials
    if (poly==T) {
      flu_form <- paste0(flu_form, "+ I(t_unit^2) + I(t_unit^3) + I(t_unit^4)")
    }
    
    ## If user-specified formula  
    if (is.null(model_form)==F) {
      return(model_form)
    } else {
      return(flu_form)
    }
  }
  fluglm.season <- function() {
    #if ==T, then default epi variable specified
    if (rlang::quo_text(season_eq)=='T') {
      
      data <- data %>%
        dplyr::mutate(epi = if_else(month(!!time_eq)>=10 | 
                                      month(!!time_eq)<=5, 
                                    T, F))  
      epi <- 'epi'
      season_eq <- quo(epi)
    } 
    
    #SEASONAL MODEL
    if (echo==T) print(paste0("Model formula: ", flu_form))
    
    base_data <- data %>%
      dplyr::filter(!!season_eq==F)
    
    #compute baseline regression 
    argslist <- list(formula=flu_form, 
                     data=base_data,
                     na.action = na.exclude,
                     ...)
    
    base_fit <- do.call(MASS::glm.nb, args=argslist)  
    pred <- predict(base_fit, 
                    newdata=data, 
                    se.fit=TRUE, 
                    type="link")
    
    upr <- pred$fit + (qnorm(1-alpha) * pred$se.fit)
    y0 <- base_fit$family$linkinv(pred$fit) #fitted values
    y0_ul <- base_fit$family$linkinv(upr)
    res <- list(base_fit = base_fit, y0 = y0, y0_ul = y0_ul) 
    return(res)
  }
  fluglm.viral <- function() {
    #VIROLOGY MODEL
    flu_form <- paste0(flu_form, 
                       "+ ", paste(viral, collapse=" + "))
    if (echo==T) {print(paste0("Model formula: ", flu_form))}
    
    ## check viral parameters are canon  
    for (i in viral) {
      if (any(sapply(data[, i], test_prop))==F) {
        warning('variable: ', i, 'exceeds boundary 0 - 1')
      }
    }
    
    #compute baseline regression 
    argslist <- list(formula=flu_form, 
                     data=data,
                     na.action = na.exclude,
                     ...)
    
    base_fit <- do.call(MASS:glm.nb, args=argslist)  
    
    ## Fitted values + prediction interval
    dta_noviral <- data
    dta_noviral[, viral] <- sapply(dta_noviral[, viral], 
                                   function(x) x=0)
    
    
    pred_noviral <- dta_noviral %>%
      predict(base_fit, 
              newdata=., 
              se.fit=TRUE, 
              type="link")
    
    upr <- pred_noviral$fit + (qnorm(1-alpha) * pred_noviral$se.fit)
    y0 <- base_fit$family$linkinv(pred_noviral$fit) #fitted values
    y0_ul <- base_fit$family$linkinv(upr) 
    res <- list(base_fit = base_fit, y0 = y0, y0_ul = y0_ul) 
    return(res)
  }
  
  ## Run Model  
  flu_form <- fluglm.model()
  if (!missing(season)) result <- fluglm.season()
  if (!missing(viral)) result <- fluglm.viral()
  
  #Report 
  if (echo==T) {
    print(summary(result$base_fit))
  }
  
  data <- data %>%
    tibble::add_column(., y0 = result$y0, y0_ul = result$y0_ul) %>%
    dplyr::select(-t_unit, -theta, -sin_f1, -cos_f1)
  
  #return results
  return(data)
}