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
flu_ex <- flumodelr::flu_ex

## ---- echo=F, results='as.is', fig.width=7.0-----------------------------
flu_fit <- ird(data=flu_ex, flu = "fludeaths", viral="prop_flupos", time="yrweek_dt")
flu_rates <- rb(data=flu_fit, flu = "fludeaths")
gr(data=flu_rates)

