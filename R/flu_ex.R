#' Sample dataset for examples. Derived from CDC 122 cities dataset.
#' 
#' The dataset is a simple year-week dataset of influenza deaths.  
#' 
#' @format A data frame with 2847 rows and 4 variables:
#' \describe{
#'   \item{year}{observation year, integer}
#'   \item{week}{observation week, integer}
#'   \item{fludeaths}{No. of deaths / 100,000 attributed to Pneumonia / Influenza}
#'   \item{yrweek_dt}{year-week-day of rate, Date}
#'   \item{epi}{an indicator T if between Oct, 1st - May, 31st}
#' }
#' @source \url{https://data.cdc.gov/dataset/Deaths-in-122-U-S-cities-1962-2016-122-Cities-Mort/mr8w-325u}
"flu_ex"