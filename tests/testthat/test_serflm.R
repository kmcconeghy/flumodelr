library(tidyverse, quietly = T, warn.conflicts = F)
library(lubridate, quietly = T, warn.conflicts = F)

context("serflm")


test_that("serflm computes correct estimates", {
  
  d <- flumodelr::fludta 
  d_pred <- serflm(data=d, outc=perc_fludeaths, time=yrweek_dt)

  #load data
  dnames <- c("year", "week", "fludeaths", "alldeaths", 
              "perc_fludeaths", "yrweek_dt", "prop_flupos",
              "epi", "y0", "y0_ul")
  
  #Tests
  expect_s3_class(d_pred, "data.frame")
  expect_that(dnames, equals(names(d_pred)))
  expect_type(d_pred$y0, "double")
  expect_type(d_pred$y0, "double")
  expect_type(d_pred$y0_ul, "double")
  expect_type(d_pred$y0_ul, "double")

})