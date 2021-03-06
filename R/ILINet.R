#' CDC Outpatient Influenza Surveillance - ILINET
#' 
#' Description from CDC Flu Portal Dashboard
#' Information on patient visits to health care providers for 
#' influenza-like illness is collected through the U.S. 
#' Outpatient Influenza-like Illness Surveillance Network, ILINet. 
#' ILINet consists of approximately 3,200 outpatient healthcare 
#' providers in all 50 states, Puerto Rico, the District of Columbia 
#' and the U.S. Virgin Islands reporting over 
#' 36 million patient visits each year. Each week, approximately 2,000 
#' outpatient healthcare providers around the country report data to 
#' CDC on the total number of patients seen and the number of those 
#' patients with influenza-like illness, ILI, by age group, 0-4 years, 
#' 5-24 years, 25-49 years, 50-64 years, and ≥ 65 years. For this system, 
#' ILI is defined as fever, temperature of 100°F, 37.8°C, or greater, 
#' and a cough and/or a sore throat in the absence of a known cause 
#' other than influenza. Sites with electronic records use an equivalent 
#' definition as determined by state public health authorities.  
#' 
#' Some variables renamed from original file for parsimony.
#' 
#' @format A data frame with 19556 rows and 7 variables:
#' \describe{
#'   \item{state}{Proper name U.S. State, character string}
#'   \item{year}{observation year, integer}
#'   \item{week}{observation week, integer}
#'   \item{ili_perc}{Unweighted percent of total Visits for ILI}
#'   \item{ili_tot}{Raw count for ILI visits}
#'   \item{providers}{No. of reporting providers}
#'   \item{patients}{No. of total patient visits}
#' }
#' @source \url{https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html}
"ilinet"