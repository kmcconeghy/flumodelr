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

## ---- results='asis', fig.width=7----------------------------------------
flu_ex <- flumodelr::flu_ex

## ---- echo=F, results='as.is', fig.width=7.0-----------------------------
ggplot(flu_ex, aes(x=yrweek_dt)) + 
  geom_line(aes(y=prop_flupos)) +
  scale_x_date(labels = date_format("%Y"), date_breaks="1 year",
               expand=c(0, .9)) + 
  xlab("Year") + 
  ylab("Proportion of influenza (+) isolates") + 
  theme_light(base_size=16)

