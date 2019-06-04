## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(flumodelr)
library(lubridate)
library(scales)
library(forecast)

## ------------------------------------------------------------------------
fludta <- flumodelr::fludta
fludta

