library(tidyverse, quietly = T, warn.conflicts = F)
library(lubridate, quietly = T, warn.conflicts = F)

context("fluglm")

test_that("Usage example works", {
  #load data
  dnames <- c("year", "week", "fludeaths", "alldeaths", 
              "perc_fludeaths", "yrweek_dt", "prop_flupos",
              "y0", "y0_ul")
  
  fludta <- flumodelr::fludta
  flu_fit <- fluglm(fludta, season=T, outc = fludeaths, time = yrweek_dt) 
  expect_s3_class(flu_fit, "data.frame")
  expect_that(dnames, equals(names(flu_fit)))
  expect_equal(nrow(flu_fit), 261L)
  expect_type(flu_fit$y0, "double")
  expect_type(flu_fit$y0_ul, "double")
})

test_that("fluglm gaussian computes", {
  d <- flumodelr::fludta 
  d_gaus <- fluglm(fludta, 
                   outc = perc_fludeaths, 
                   season=T,
                   time = yrweek_dt, 
                   family = gaussian) 
  #load data
  dnames <- c("year", "week", "fludeaths", "alldeaths", 
              "perc_fludeaths", "yrweek_dt", "prop_flupos",
              "y0", "y0_ul")
  
  #Tests
  expect_s3_class(d_gaus, "data.frame")
  expect_that(dnames, equals(names(d_gaus)))
  expect_equal(nrow(d_gaus), 261L)
  expect_type(d_gaus$y0, "double")
  expect_type(d_gaus$y0_ul, "double")
})

test_that("fluglm poisson computes", {
  d <- flumodelr::fludta 
  d_pois <- fluglm(fludta, 
                   outc = fludeaths, 
                   time = yrweek_dt, 
                   season=T,
                   family = poisson,
                   offset = log(alldeaths)) 
  #load data
  dnames <- c("year", "week", "fludeaths", "alldeaths", 
              "perc_fludeaths", "yrweek_dt", "prop_flupos",
              "y0", "y0_ul")
  
  #Tests
  expect_s3_class(d_pois, "data.frame")
  expect_equal(nrow(d_pois), 261L)
  expect_that(dnames, equals(names(d_pois)))
  expect_type(d_pois$y0, "double")
  expect_type(d_pois$y0_ul, "double")
})

test_that("fluglm viral computes", {
  d <- flumodelr::fludta 
  d_viral <- fluglm(fludta, 
                   outc = fludeaths, 
                   time = yrweek_dt, 
                   family = poisson,
                   offset = log(alldeaths),
                   viral="prop_flupos") 
  #load data
  dnames <- c("year", "week", "fludeaths", "alldeaths", 
              "perc_fludeaths", "yrweek_dt", "prop_flupos",
              "y0", "y0_ul")
  
  #Tests
  expect_s3_class(d_viral, "data.frame")
  expect_equal(nrow(d_viral), 261L)
  expect_that(dnames, equals(names(d_viral)))
  expect_type(d_viral$y0, "double")
  expect_type(d_viral$y0_ul, "double")
})