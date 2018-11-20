#' @title ird: Perform incidence rate-difference models
#' 
#' @description Calculates the rate difference between influenza season 
#' and peri-influenza season, or between influenza season and
#' summer season
#' 
#' @usage ird(data=NULL, outc=NULL, time=NULL, viral, 
#'             period=52, respStart=27, high=0.1, fluStart=40,
#'             fluStop=18, echo=F)
#' 
#' @param data A dataframe class object, must contain time variable, 
#' epidemic indicator, and measure of influenza morbidity
#' 
#' @param outc A named variable in 'data' dataframe, identifies 
#' a numeric type variable in dataframe data which is the measure 
#' of disease morbidity / mortality
#' 
#' @param time A named variable in 'data' dataframe, identifies 
#' a numeric/integer class variable in dataframe which corresponds 
#' to a unit of time, must be unique (i.e. non-repeating)  
#' 
#' @param period a numeric variable indicating the period length of the 
#' time variable (i.e. weeks = 52, the default)  
#' 
#' @param respStart A numeric/integer class variable, must specify the 
#' week number of the start of the respiratory season.
#' The default value is 27. This corresponds with the beginning of July.
#' 
#' @param viral A named variable in 'data' data.frame, identifies
#' a numeric type variable in dataframe data which is the measure
#' of viral activity. 
#' 
#' @param high A numeric class variable, must specify the
#' proportion of positive isolates for a week to be considered of
#' "high viral activity". The default value is 0.10. 
#'   
#' @param fluStart A numeric/integer class variable, must specify the
#' week number of the start of the influenza season. The default 
#' value is 40. This corresponds with the beginning of October.
#' 
#' @param fluStop A numeric/integer class variable, must specify the
#' week number of the start of the influenza season. The default 
#' value is 18. This corresponds with the end of April.
#'   
#' @return an object of class data.frame
#' 
#' @export
#' 
#' @examples
#' require(flumodelr)
#' fludta <- flumodelr::fludta
#' flu_fit <- ird(data=fludta, outc = perc_fludeaths, viral=prop_flupos, time=yrweek_dt)
#' flu_rates <- rb(flu_fit, perc_fludeaths)
#'               
#' flu_rates
#' 
#' @references 
#' Thompson WW, Weintraub E, Dhankhar P, Cheng PY, Brammer L, 
#' Meltzer MI, Bresee JS, Shay DK. Estimates of US influenza‐associated 
#' deaths made using four different methods. Influenza and other 
#' respiratory viruses. 2009 Jan 1;3(1):37-49.
#' /url{http://onlinelibrary.wiley.com/doi/10.1111/j.1750-2659.2009.00073.x/full} 
#' 
#' @import rlang dplyr
#' 
ird <- function(data=NULL, 
                outc=NULL, 
                time=NULL,
                viral,
                period=52,
                respStart=27,
                high=0.1,
                fluStart=40,
                fluStop=18,
                echo=F
                ) {
  #Set-up
    if (echo==T) { #only print for diagnostic purposes
    cat("incidence rate-difference model \n",
        paste0(rep("=", 60), collapse=""), "\n")
    }
  
  #sanity checks
  #df is data.frame
    stopifnot(is.data.frame(data)) 
  
  #tidy evaluation  
    outc_eq <- enquo(outc)
    time_eq <- enquo(time)
    viral_eq <- enquo(viral)
  
    if (missing(viral)) {
      viral <- "missing"
      viral_eq <- quo(viral)
    } else { viral_eq <- enquo(viral) }
    
  data <- data %>% dplyr::arrange(., !!time_eq)
  
  
  #parameters  
  if (echo==T) { #only print for diagnostic purposes
    cat("Setting ird parameters...\n")
    cat(" 'outc' argument is:", rlang::quo_text(outc_eq), "\n")
    cat(" 'time' variable is:", rlang::quo_text(time_eq), "\n")
    cat(" 'viral' variable is:", rlang::quo_text(viral_eq), "\n")
    cat("  time period is:", period, "\n")
  }
  
  #find respiratory seasons
    findseason <- function (calyear, calweek, respStart=27){
      if (calweek >= respStart) {return(calyear)} else {return(calyear-1)}
    }

    data$season <- mapply(findseason, data$year, data$week)
    
  #compute rate differences  
  
    #find high versus low viral activity periods
    findhigh <- function (prop, high=0.1){
      return(prop >= high)
    }
  
    #
    if(!missing(viral)) data$high <- mapply(findhigh, data[rlang::quo_text(viral_eq)])
    
    #find influenza versus summer baseline period
    findflu <- function (calweek, fluStart=40, fluStop=18){
      (calweek >=fluStart | calweek <=fluStop)
    }
    
    data$fluseason <- mapply(findflu, data$week)
    
  #return results
  return(data)
  
}

#' @title rb: Compute differences in ird model
#' 
#' @description Computes the marginal mean difference across influenza season
#' 
#' @usage rb(data, outc, echo=F)
#' @export  
#' @import rlang dplyr
#' 
rb <- function(data, outc, echo=F){ 
  
  #tidy evaluation  
    outc_eq <- enquo(outc)

  #parameters 
    if (echo==T) {
      cat("Setting rb parameters...\n")
      cat(" 'outc' argument is:", rlang::quo_text(outc_eq), "\n")
    }  
  
  #sum the outcomes
    highrates <- data %>% 
      group_by(season, high) %>% 
      summarize(out_high = mean(!!outc_eq))
  
    flurates <- data %>% 
      group_by(season, fluseason) %>% 
      summarize(out_flu = mean(!!outc_eq))
  
  #join the tables
    flu_rates <- flurates %>% 
      left_join(highrates, by = c("fluseason"="high", "season"))
    
    names(flu_rates) <- c("season", "high_act", 
                          paste0(rlang::quo_text(outc_eq), "_fluseason"), 
                          paste0(rlang::quo_text(outc_eq),"_viral_act"))
    levels(flu_rates$high_act) <- c(TRUE,FALSE)
    return(flu_rates)
}
#' @title gr: Bar graph for ird models  
#' 
#' @description Outputs simple bar chart for incidence rate difference
#' models  
#' 
#' @param data A formatted dataset created irb() rb() commands
#' @usage gr(data)
#' @export  
#' @import ggplot2
gr <- function(data){
  ggplot(data, aes(x = season, y = perc_fludeaths_fluseason, fill=high_act))+
    geom_bar(stat="identity", position = position_dodge(), colour="black")
}



               