#' Deaths in 122 U.S. cities - 1962-2016. 122 Cities Mortality Reporting System.
#'
#' CDC Site description:
#' This file contains the complete set of data reported to 
#' 122 Cities Mortality Reposting System. The system was 
#' retired as of 10/6/2016. While the system was running each 
#' week, the vital statistics offices of 122 cities across 
#' the United States reported the total number of death 
#' certificates processed and the number of those for 
#' which pneumonia or influenza was listed as the underlying 
#' or contributing cause of death by age group (Under 28 
#' days, 28 days - 1 year, 1-14 years, 15-24 years, 25-44 
#' years, 45-64 years, 65-74 years, 75-84 years, and - 85 years). 
#' U:Unavailable. - : No reported cases.
#' * Mortality data in this table were voluntarily 
#' reported from 122 cities in the United States, 
#' most of which have populations of >100,000. 
#' A death is reported by the place of its occurrence and 
#' by the week that the death certificate was filed. 
#' Fetal deaths are not included. Total includes unknown ages. 
#' Date last obtained: 10/27/2017
#' 
#' The dataset is in a "panel" format, city by year-week.
#' 
#' Some variables renamed from original file for parsimony.
#' 
#' @format A data frame with 346342 rows and 8 variables:
#' \describe{
#'   \item{region}{Referral region}
#'   \item{state}{Postal code for U.S. State, character string}
#'   \item{city}{Proper name of U.S. city, character string}
#'   \item{year}{observation year, integer}
#'   \item{week}{observation week, integer}
#'   \item{deaths_pnaflu}{No. of deaths attributed to Pneumonia / Influenza}
#'   \item{deaths_allcause}{No. of deaths due to any cause}
#'   \item{deaths_65older}{No. of deaths due to any cause in those 65 years or older}
#' }
#' @source \url{https://data.cdc.gov/dataset/Deaths-in-122-U-S-cities-1962-2016-122-Cities-Mort/mr8w-325u}
"cdc122city"