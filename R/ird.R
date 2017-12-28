#' @title ird: Perform incidence rate-difference models
#' 
#' @description Calculates the rate difference between influenza season 
#' and peri-influenza season, or between influenza season and
#' summer season
#' 
#' @usage ird (data=NULL, flu=NULL, time=NULL, viral=NULL, 
#'             t.interval="wofy", respStart=27, high=0.1, fluStart=40,
#'             fluStop=18)
#' 
#' @param data A dataframe class object, must contain time variable, 
#' epidemic indicator, and measure of influenza morbidity
#' 
#' @param flu A character string of length=1, identifies 
#' a numeric type variable in dataframe data which is the measure 
#' of disease morbidity / mortality
#' 
#' @param time A character string of length=1, identifies 
#' a numeric/integer class variable in dataframe which corresponds 
#' to a unit of time, must be unique (i.e. non-repeating)  
#' 
#' @param t.interval A character string, must specify whether 
#' unit of time cycle is weeks/years ("wofy", i.e. 52 weeks), 
#' or month/years ("mofy"), days/years("dofy")  
#' 
#' @param respStart A numeric/integer class variable, must specify the 
#' week number of the start of the respiratory season.
#' The default value is 27. This corresponds with the beginning of July.
#' 
#' @param viral A character string of length=1, identifies
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
#' flu_fit <- ird(data=fludta, flu = "perc_fludeaths", viral="prop_flupos", time="yrweek_dt")
#' flu_rates <- rb(flu_fit, "perc_fludeaths")
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
ird <- function(data=NULL, 
                flu=NULL, 
                time=NULL,
                viral=NULL,
                t.interval="wofy",
                respStart=27,
                high=0.1,
                fluStart=40,
                fluStop=18
                ) {
  cat("incidence rate-difference model \n",
      paste0(rep("=", 60), collapse=""), "\n")
  
  #internal objects  
    t.intervals = c("wofy"=52, "mofy"=12, "dofy"=365)
      
  #sanity checks
    #df is data.frame
      if (is.data.frame(data)==F) {
        stop("Data object is not a data.frame class object")
      }
    #flu variable exists and valid  
      if (is.na(flu)==T) {
        stop("influenza variable not specified, must specify, see ?serflm")
      }
      if (flu %in% names(data)==F) {
        stop("flu variable not named in data object, see ?serflm")
      }
      if (is.numeric(data[[flu]])==F) {
        stop("flu variable not type numeric, see ?serflm")
      }
    
       #time variable
      if (is.na(time)==T) {
        stop("time variable not specified, must specify, see ?serflm")
      }
      if (time %in% names(data)==F) {
        stop("time variable not named in data object, see ?serflm")
      }
      if (n_distinct(time)!= length(time)) {
        stop("time variable repeats, should be unique, see ?serflm")
      }
      if (t.interval %in% names(t.intervals)==F) {
        stop("time interval not a valid option, see ?serflm")
      }
  ntime <- as.name(time)
  data <- data %>% arrange(., !!ntime)
    
  #parameters  
  cat("Setting ird parameters...\n")
    cat(" 'time' variable is:", time, "\n")
    
    t_interval <- t.intervals[t.interval]
    cat("  time interval is:", t_interval, "\n")
    cat("  flu argument is:", flu, "\n")
 
    #build model formula
    
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
  
    data$high <- mapply(findhigh, data$prop_flupos)
    
    #find influenza versus summer baseline period
    findflu <- function (calweek, fluStart=40, fluStop=18){
      (calweek >=fluStart | calweek <=fluStop)
    }
    
    data$fluseason <- mapply(findflu, data$week)
    
  #return results
  return(data)
  
}
#' @export  
rb <- function(data, flu){ #calculate the rates for each of the periods
  #prepare arguments to work in dplr
  flu_sym <- rlang::sym(flu)
  
  #parameters 
cat("Setting rb parameters...\n")
  cat("  flu argument is:", flu_sym, "\n")
  
  #sum the outcomes
  highrates <- data %>% group_by(season, high) %>% summarize(out_high = mean(!!flu_sym))
  flurates <- data %>% group_by(season, fluseason) %>% summarize(out_flu = mean(!!flu_sym))
  
  #join the tables
  flu_rates <- flurates %>% left_join(highrates, by = c("fluseason"="high", "season"))
  names(flu_rates) <- c("season","high_act",paste0(flu,"_fluseason"),paste0(flu,"_viral_act"))
  levels(flu_rates$high_act) <- c(TRUE,FALSE)
  return(flu_rates)
}

#' @export  
gr <- function(data){
  ggplot(data, aes(x = season, y = perc_fludeaths_fluseason, fill=high_act))+
    geom_bar(stat="identity", position = position_dodge(), colour="black")
}



               