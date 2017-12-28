## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(flumodelr)
library(tibble)

## ------------------------------------------------------------------------
fludta <- flumodelr::fludta

## ---- echo=F, results='as.is', fig.width=7.0-----------------------------
flu_fit <- ird(data=fludta, flu = "perc_fludeaths", viral="prop_flupos", time="yrweek_dt")
flu_rates <- rb(data=flu_fit, flu = "perc_fludeaths")
gr(data=flu_rates)

