library(tidyverse, quietly = T, warn.conflicts = F)
library(lubridate, quietly = T, warn.conflicts = F)

context("flum")

test_that("flum computes incidence rate difference", {
  d <- flumodelr::fludta 
  d_ird <- flum(fludta, model="ird", 
                 outc=perc_fludeaths, time=yrweek_dt) 
  #load data
  dnames <- c("year", "week", "fludeaths", "alldeaths", 
              "perc_fludeaths", "yrweek_dt", "prop_flupos",
              "epi", "y0", "y0_ul")
  
  #Tests
  expect_s3_class(d_gaus, "data.frame")
  expect_that(dnames, equals(names(d_gaus)))
  expect_type(d_gaus$y0, "double")
  expect_type(d_gaus$y0_ul, "double")
})

fludta <- flumodelr::fludta  

fludta_mod <- flum(fludta, model="ird", 
                   outc=perc_fludeaths, time=yrweek_dt)