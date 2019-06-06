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
#' flu_fit <- ird(data=fludta, viral=prop_flupos, time=yrweek_dt)
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
                viral=NULL,
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
    high_eq <- enquo(high)
  
    if (missing(viral)) {
      viral_eq <- "missing"
    } else { viral_eq <- enquo(viral) }
    
  data <- data %>% dplyr::arrange(., !!time_eq)
  
  
  #parameters  
  if (echo==T) { #only print for diagnostic purposes
    cat("Setting ird parameters...\n")
    cat(" 'outc' argument is:", rlang::quo_text(outc_eq), "\n")
    cat(" 'time' variable is:", rlang::quo_text(time_eq), "\n")
    cat(" 'viral' variable is:", rlang::quo_text(viral_eq), "\n")
    cat(" 'high' variable is:", rlang::quo_text(high_eq), "\n")
    cat("  time period is:", period, "\n")
  }
  
  #find respiratory seasons
    findseason <- function (calyear, calweek, respStart=27){
      if (calweek >= respStart) {return(calyear)} else {return(calyear-1)}
    }

    data$season <- mapply(findseason, data$year, data$week)
    
  #Identify observation versus baseline periods 
  
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
rb <- function(data=NULL, outc=NULL, echo=F){ 
  
  #tidy evaluation  
    outc_eq <- enquo(outc)

  #parameters 
    if (echo==T) {
      cat("Setting rb parameters...\n")
      cat(" 'outc' argument is:", rlang::quo_text(outc_eq), "\n")
    }  
  
  #sum the outcomes
    if("high" %in% names(data)) {highrates <- data %>% select(season, high, !!outc_eq) %>% group_by(season, high) %>% 
                        summarize(out_high = sum(!!outc_eq))} #
  
    flurates <- data %>%  select(season, fluseason, !!outc_eq) %>% group_by(season, fluseason) %>%
                          summarize(out_flu = sum(!!outc_eq))
  
  #join the tables
    if("high" %in% names(data)){
    flu_rates <- flurates %>% left_join(highrates, by = c("fluseason"="high", "season"))
    
    names(flu_rates) <- c("season", "period", 
                          paste0(rlang::quo_text(outc_eq), "_fluseason"), 
                          paste0(rlang::quo_text(outc_eq),"_viral_act"))}
    
    else{flu_rates <- flurates
    names(flu_rates) <- c("season", "period", 
                          paste0(rlang::quo_text(outc_eq), "_fluseason"))}
    
    flu_rates$period <- factor(if_else(flu_rates$period,"High","Low"))
    flu_rates <- data.frame(flu_rates)

  
    
   #calculate excess outcomes
    period.high <- flu_rates %>% dplyr::filter(period == "High")
    period.low <- flu_rates %>% dplyr::filter(period == "Low")
    period.all <- period.high %>% inner_join(period.low, by = "season")
    excess1 <- period.all[3] - period.all[6]
    if("high" %in% names(data)){excess2 <- period.all[4] - period.all[7]}
    
   #add excess rates to flu_rates
    period <- factor(rep("Excess", nrow(period.all)), levels = c("High", "Low", "Excess"))
    if("high" %in% names(data)){add <- data.frame(period.all[1], period, excess1, excess2)}
    else {add <- data.frame(period.all[1], period, excess1)}
    names(add) <- names(flu_rates)
    flu_rates <- rbind(add, flu_rates)
    flu_rates <- as_tibble(flu_rates) %>% dplyr::arrange(season)
 
return(flu_rates)  

}
#' @title gr: Line graph for ird models  
#' 
#' @description Outputs simple line graph for incidence rate difference
#' models  
#' 
#' @param data A formatted dataset created irb() rb() commands
#' @usage gr(data)
#' @export  
#' @import ggplot2
gr <- function(data, outc, echo=F){
  
  #tidy evaluation  
  outc_eq <- enquo(outc)
  
  #parameters 
  if (echo==T) {
    cat("Setting rb parameters...\n")
    cat(" 'outc' argument is:", rlang::quo_text(outc_eq), "\n")
  } 
  
  theme_set(theme_bw())
  outc <- rlang::quo_text(outc_eq)
  data$season <- as.Date(paste("1jan",data$season,sep=""), "%d%b%Y")
  brks <- data$season[seq(1, length(data$season), 2)]
  lbls <- lubridate::year(brks)

  #define base graph
  g <- ggplot(data, aes(x=season)) +
        geom_line(aes(y = !!outc_eq, col = period)) +
        labs(title="Incidence rate-difference", 
         subtitle=outc, 
         y="Number of outcomes", 
         color=NULL) +  # title and caption
        scale_x_date(labels = lbls, breaks = brks) +
        scale_color_manual(labels = c("High", "Low", "Excess"), values = c("High"="blue", "Low"="green", "Excess" = "red")) +
        theme(axis.text.x = element_text(angle = 0, vjust=0.5, size = 8),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid
 
  return(g)
  }


