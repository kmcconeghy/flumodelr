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
fludta %>% select(year,week,alldeaths,prop_flupos,week)

## ------------------------------------------------------------------------
fludta_mod <- ird(data =fludta, 
                  outc =alldeaths, 
                  viral=prop_flupos, 
                  time =week,
                  echo =T)

## ------------------------------------------------------------------------
fludta_mod %>% select(year,week,alldeaths,prop_flupos,week, season, high, fluseason)

## ------------------------------------------------------------------------
fludta_rates <- rb(data =fludta_mod, outc =alldeaths, echo = T)

## ------------------------------------------------------------------------
fludta_rates

## ---- fig.width=7--------------------------------------------------------
gr(data =fludta_rates, outc =alldeaths_fluseason, echo = FALSE )

