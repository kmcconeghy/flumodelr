sim_fluseason <- function(type_a = c(100, 95), 
                          b_season = F, 
                          type_b = c(4, 7),
                          peak_cnt = 1000) {
  week_density <- seq(1, 32, 1) / 32
  
  week_density_flusim <- dbeta(week_density, 4, 7)*0.3 + 
    dbeta(week_density, 200, 190)*0.7
  
  
  
  
}