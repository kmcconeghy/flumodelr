## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(dplyr)
library(ggplot2)

## ------------------------------------------------------------------------
cdc122 <- (flumodelr::cdc122city)  
dim(cdc122)
cdc122

## ------------------------------------------------------------------------
nrevss <- (flumodelr::nrevss)  
dim(nrevss)
nrevss

## ------------------------------------------------------------------------
ilinet <- (flumodelr::ilinet)  
dim(ilinet)
ilinet

## ---- eval=F-------------------------------------------------------------
#  install.packages("EpiWeek")

## ------------------------------------------------------------------------
library(lubridate) #For working with dates  
EpiWeekToDates <- function(year, weeknum) {
  
  #Compute 4th day in January
    jan4 <- ymd(paste(year, 1, 4, sep="-"))
  
  #Compute day of week
    DofW <- wday(jan4, week_start = 7)-1 #Sunday start
    #The -1 is  correction for how wday() counts days  
    
  #If Jan 4th was Sun, starting week date is 01/04/xxxx
    startweek <- if_else(DofW == 7, jan4, (jan4 - (DofW)))
 
  #First Date of Week
    d0 = startweek + (weeknum - 1) * 7
  
  #Last Date of Week
    d1 = startweek + (weeknum - 1) * 7 + 6 
  
  return(list(d0 = ymd(d0), d1 = ymd(d1)))
}

## ------------------------------------------------------------------------
example <- cdc122 %>%
  mutate(FirstDateOfWeek = EpiWeekToDates(year, week)[[1]])

example %>% select(year, week, FirstDateOfWeek) %>% 
  distinct() %>% 
  filter(year>2010)

