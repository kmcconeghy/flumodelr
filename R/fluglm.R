#' @title fluglm: Generate fitted prediction values with generalized linear model. 
#' 
#' @description Similar to serflm, but allows for more 
#' specifications. 
#'
#' @usage fluglm(data=NULL, outc=NULL, epi=NULL, time=NULL, 
#'               t.interval=52, echo=F, poly=T, model.form=NULL,
#'               int_type="ci", alpha=0.1)
#'               
#' @param data A dataframe class object, must contain time variable, 
#' epidemic indicator, and measure of influenza morbidity
#' 
#' @param outc an unquoted name of a column in data which corresponds to 
#' the outcome variable of interest
#' 
#' @param epi an unquoted name of a column in data object (e.g. epi) 
#' or if null will default to Sept - May.  
#' 
#' @param time an unquoted name of a column in data object, must be 
#' a numeric/integer class variable in dataframe which corresponds 
#' to a unit of time, must be unique (i.e. non-repeating)  
#' 
#' @param period a numeric vector indicating period length, i.e. 52 weeks in year  
#'   
#' @param echo A logical parameter, if T will print variables used in model.  
#' 
#' @param poly A logical parameter, if T will include a quadratic, cubic and 
#' quartic term.  
#' 
#' @param model.form An object of type formula, allowing for user-specified 
#' model to be passed on to glm(). Default missing.    
#' 
#' @param int_type Specifies type of upper interval to be output, currently
#' only allows for confidence intervals. Predictional intervals to be added,  
#' but are only approximate for Poisson families.  
#' 
#' @param alpha The threshold for interval, default is 0.1 
#' (a one-sided 95% interval).  
#' 
#' @param offset Specify if offset term to be used, must specify log(object)  
#' 
#' @param ... options passed on to glm model (e.g. family=poisson, see ?glm)  
#'   
#' @return an object of class data.frame, input + y0 (fitted values), y0_ul 
#' the upper threshold
#' 
#' @export
#' 
#' @examples
#' require(flumodelr)
#' fludta <- flumodelr::fludta
#' flu_fit <- fluglm(fludta, outc = fludeaths, time = yrweek_dt)  
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
fluglm <- function(data=NULL, outc=NULL, epi=NULL, time=NULL, 
                   period=52, echo=F, poly=T, model.form=NULL, 
                   int_type="ci", alpha=0.1, offset=NULL, ...) {
  #sanity checks
  #df is data.frame
  stopifnot(is.data.frame(data)) 
  
  #tidy evaluation  
  outc_eq <- enquo(outc)
  time_eq <- enquo(time)
  epi_eq <- enquo(epi)
  offset_eq <- enquo(offset)  
  
  #If no epi variable then, generate automatic period from Sept - May. 
  #write epi object as name
    if (epi_eq==quo(NULL)) {
      data <- data %>%
        dplyr::mutate(epi = if_else(month(!!time_eq)>=10 | 
                                      month(!!time_eq)<=5, 
                                    T, F))  
      epi <- "epi"
      epi_eq <- quo(epi)
    } else {epi_eq <- enquo(epi)}
  
  #parameters  
    if (echo==T) {
      cat("Setting regression parameters...\n")
      cat(" 'outc' variable is:", rlang::quo_text(outc_eq), "\n")
      cat(" 'epi' variable is:", rlang::quo_text(epi_eq), "\n")
      cat(" 'time' variable is:", rlang::quo_text(time_eq), "\n")
      cat("  time period is:", period, "\n")
    }
    
    data <- data %>% dplyr::arrange(., UQ(time_eq))
    
    data <- data %>%
      dplyr::mutate(t_unit = row_number(),
                    theta = 2*t_unit/period,
                    sin_f1 = sinpi(theta),
                    cos_f1 = cospi(theta))
  
  
  #build model formula
    flu.form <- paste0(rlang::quo_text(outc_eq), 
                       " ~ ", "t_unit", "+ sin_f1", "+ cos_f1")
    ## Offset term
    if (offset_eq!=quo(NULL)) {
      flu.form <- paste0(flu.form, 
                         "+ offset(", rlang::quo_text(offset_eq), ")")
    }
  
    ## Add polynomials
    if (poly==T) {
      flu.form <- paste0(flu.form, "+ I(t_unit^2) + I(t_unit^3) + I(t_unit^4)")
    }
  
    ## If user-specified formula  
      if (is.null(model.form)==F) {
        flu.form <- model.form
      }
  
  if (echo==T) {print(paste0("Model formula: ", flu.form))}
  
  #compute baseline regression 
    argslist <- list(formula=flu.form, 
                 data=dplyr::filter(data, UQ(epi_eq)==F),
                 na.action = na.exclude,
                 ...)
    
  base_fit <- do.call(glm, args=argslist)  
  
  #parameters  
  if (echo==T) {
    print(summary(base_fit))
  }
  
  ## Fitted values + prediction interval
  if (int_type == "ci") {
    pred <- data %>%
      predict(base_fit, 
              newdata=., 
              se.fit=TRUE, 
              type="link")
    
    upr <- pred$fit + (qnorm(1-alpha/2) * pred$se.fit)
    y0 <- base_fit$family$linkinv(pred$fit) #fitted values
    y0_ul <- base_fit$family$linkinv(upr)
  }
  data <- data %>%
    tibble::add_column(., y0, y0_ul) %>%
    dplyr::select(-t_unit, -theta, -sin_f1, -cos_f1)
  
  #return results
  return(data)
}