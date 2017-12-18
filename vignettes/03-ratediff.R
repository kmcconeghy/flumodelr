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


