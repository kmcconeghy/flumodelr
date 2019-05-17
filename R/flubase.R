#' @title flubase: An internal function for taking the average of previous 
#' baseline seasons  
#' 
#' @description A simple function will compute the mean by group, across a time unit
#'
#' @usage flubase(data, outc, time, season)
#'     
#' @param data A dataset which contains a influenza measure, time and grouping 
#' variable, season
#'         
#' @param outc Influenza measure  
#' 
#' @param time A a numeric vector with influenza measure  
#' 
#' @param group An vector ordered by time which indicates grouping by season '
#' 
#' @param k The order of lookback, if k=5 then will take mean of previous 5 
#' groups (i.e. influenza seasons)
#' 
#' @return a vector of length equal to x
#' 
#' @export
#' 
#' @import rlang dplyr
#'
flubase <- function(data, outc, time, group, k=5) {
  time_eq <- enquo(time)
  outc_eq <- enquo(outc)
  group_eq <- enquo(group)
  
  n_k <- n_distinct(data[, quo_name(group_eq)])
  if(n_k<k) stop('Number of groups less than look-back specified, default 5')
  
  k_groups <- distinct(data, !!group_eq)[1:k, ]

  data_2 <- data %>%
    inner_join(., k_groups, by = rlang::quo_as_text(group_eq)) %>%
    group_by(!!time_eq) %>%
    summarize(!!outc_eq := mean(!!outc_eq, na.rm=T))
  
  return(data_2)
}