## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(tidyverse)
library(lubridate)
library(scales)
library(flumodelr)

## ------------------------------------------------------------------------
fludta <- flumodelr::fludta

## ------------------------------------------------------------------------
fludta_mod <- ird(data =fludta, 
                  outc =perc_fludeaths, 
                  viral=prop_flupos, 
                  time =yrweek_dt,
                  echo=T)

## ------------------------------------------------------------------------
fludta_rates <- rb(data=fludta_mod, outc = perc_fludeaths)

## ------------------------------------------------------------------------
gr(data=fludta_rates)

