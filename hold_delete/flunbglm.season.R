flunbglm.season <- function() {
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
  return(get_fitvals(base_fit, data))
   
}