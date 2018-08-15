#' @title fluserf: Fit a Serfling Model on Time Series Data
#' 
#' @description Performs a cyclical linear regression model
#'
#' @usage fluserf(data=NULL, outc=NULL, epi=NULL, time=NULL, 
#'               period=52, echo=F)
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
#' @param echo A logical parameter, if T. Will print variables used in model.  
#' 
#' @param alpha Specify level for one-sided test to compute upper limit interval. 
#' default is 0.05
#' 
#' @return an object of class data.frame, input + y0 (fitted values), y0_ul 
#' the upper serfling threshold
#' 
#' @export
#' 
#' @examples
#' require(flumodelr)
#' fludta <- flumodelr::fludta
#' flu_fit <- fluserf(fludta, outc = fludeaths, time = yrweek_dt)  
#'               
#' head(flu_fit)
#' 
#' @references 
#' Serfling RE. Methods for current statistical analysis of 
#' excess pneumonia-influenza deaths. Public Health Rep. 1963 Jun; 
#' 78(6): 494 - 506.  
#' /url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1915276/} 
#' 
#' @import rlang dplyr magrittr
#' 
fluserf <- function(data=NULL, outc=NULL, epi=NULL, time=NULL, 
                   period=52, echo=F, alpha=0.05) {
  #sanity checks
    #df is data.frame
      stopifnot(is.data.frame(data)) 
      
    #tidy evaluation  
      outc_eq <- enquo(outc)
      time_eq <- enquo(time)
      epi_eq <- enquo(epi)

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
    flu.form <- as.formula(
      paste0(rlang::quo_text(outc_eq), " ~ ", "t_unit", "+ sin_f1", "+ cos_f1"))
  
  #compute baseline regression  
    base_fit <- data %>%
      dplyr::filter(UQ(epi_eq)==F) %>% #Must UQ, !! doesnt work
      glm(flu.form, family=gaussian, data=., na.action = na.exclude)  
    
    #parameters  
    if (echo==T) {
      print(base_fit)
    }
    
  ## Fitted values + prediction interval
    ## Fitted values + prediction interval
    pred <- data %>%
      predict(base_fit, 
              newdata=., 
              se.fit=TRUE, 
              type="link")
    
    y0 <- pred$fit #fitted values
    y0_ul <- pred$fit + (qnorm(1-alpha) * pred$se.fit)

    data <- data %>%
      tibble::add_column(., y0, y0_ul) %>%
      dplyr::select(-t_unit, -theta, -sin_f1, -cos_f1)
    
    #return results
    return(data)
}

      