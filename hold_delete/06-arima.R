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

flu_deaths <- fludta$fludeaths

## ------------------------------------------------------------------------
ggAcf(flu_deaths, main='Autocorrelation plot')

## ------------------------------------------------------------------------
ggPacf(flu_deaths, main='Partial autocorrelation plot')

## ------------------------------------------------------------------------
arima(flu_deaths, order=c(2, 0, 0))


## ------------------------------------------------------------------------
auto.arima(flu_deaths, stepwise=F, approximation=F)

## ------------------------------------------------------------------------
ggtsdisplay(flu_deaths)

## ------------------------------------------------------------------------
flu_deaths %>% diff() %>% ggtsdisplay(., main='First Difference, yt\'')

## ------------------------------------------------------------------------
fit <- Arima(flu_deaths, order=c(1, 0, 4), seasonal=c(0,1,1))
fit

## ------------------------------------------------------------------------
fit %>% residuals() %>% ggtsdisplay()

## ------------------------------------------------------------------------
Arima(flu_deaths, order=c(0, 1, 5), seasonal=c(0,1,1)) %>% residuals() %>% ggtsdisplay()

## ------------------------------------------------------------------------
Arima(flu_deaths, order=c(0, 1, 5), seasonal=c(0,1,1)) %>% checkresiduals()


## ------------------------------------------------------------------------
fit <- flurima(fludta, outc = fludeaths, time=week_in_order, echo=T)

## ------------------------------------------------------------------------
sessioninfo::session_info()

