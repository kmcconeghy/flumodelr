## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(dplyr)
library(ggplot2)
library(flumodelr)

## ------------------------------------------------------------------------
cdc122 <- (flumodelr::cdc122city)  
dim(cdc122)
head(cdc122)

## ------------------------------------------------------------------------
nrevss <- (flumodelr::nrevss)  
dim(nrevss)
head(nrevss)

## ------------------------------------------------------------------------
ilinet <- (flumodelr::ilinet)  
dim(ilinet)
head(ilinet)

## ------------------------------------------------------------------------
flumodelr::epiweek_dt

## ---- eval=F-------------------------------------------------------------
#  install.packages("EpiWeek")

## ------------------------------------------------------------------------
library(lubridate) #For working with dates  

## ------------------------------------------------------------------------
example <- cdc122 %>%
  mutate(FirstDateOfWeek = epiweek_dt(year, week)[[1]])

example %>% select(year, week, FirstDateOfWeek) %>% 
  distinct() %>% 
  dplyr::filter(year>2010) %>%
  head(.)

