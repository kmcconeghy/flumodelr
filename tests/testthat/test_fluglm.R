library(tidyverse, quietly = T, warn.conflicts = F)
library(lubridate, quietly = T, warn.conflicts = F)

context("fluglm")

test_that("Usage example works", {
  fludta <- flumodelr::fludta
  flu_fit <- fluglm(fludta, outc = fludeaths, time = yrweek_dt) 
  expect_is(flu_fit, 'data.frame')
})

test_that("fluglm gaussian computes", {
  d <- flumodelr::fludta 
  d_gaus <- fluglm(fludta, 
                   outc = perc_fludeaths, 
                   time = yrweek_dt, 
                   family = gaussian) 
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

test_that("fluglm poisson computes", {
  d <- flumodelr::fludta 
  d_pois <- fluglm(fludta, 
                   outc = fludeaths, 
                   time = yrweek_dt, 
                   family = poisson,
                   offset = log(alldeaths)) 
  #load data
  dnames <- c("year", "week", "fludeaths", "alldeaths", 
              "perc_fludeaths", "yrweek_dt", "prop_flupos",
              "epi", "y0", "y0_ul")
  
  #Tests
  expect_s3_class(d_pois, "data.frame")
  expect_that(dnames, equals(names(d_pois)))
  expect_type(d_pois$y0, "double")
  expect_type(d_pois$y0_ul, "double")
})