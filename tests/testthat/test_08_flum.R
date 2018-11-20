library(tidyverse, quietly = T, warn.conflicts = F)
library(lubridate, quietly = T, warn.conflicts = F)
fludta <- flumodelr::fludta

context("flum")
test_that("flum computes incidence rate difference", {
  
  
  d <- flum(fludta, model="ird", 
            outc=perc_fludeaths, time=yrweek_dt,
            viral=prop_flupos)
  
  #load data
  dnames <- c("year", "week", "fludeaths", "alldeaths", 
              "perc_fludeaths", "yrweek_dt", "prop_flupos",
              "season", "high", "fluseason")
  
  #Tests
  expect_s3_class(d, "data.frame")
  expect_that(dnames, equals(names(d)))
  expect_equal(nrow(d), 261L)
  expect_type(d$high, "logical")
  expect_type(d$fluseason, "logical")
  
})

test_that("flum computes fluserf model", {
  d <- flum(fludta, model="fluserf", 
            outc=perc_fludeaths, time=yrweek_dt)
  #load data
  dnames <- c("year", "week", "fludeaths", "alldeaths", 
              "perc_fludeaths", "yrweek_dt", "prop_flupos",
              "epi", "y0", "y0_ul")
  
  #Tests
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 261L)
  expect_that(dnames, equals(names(d)))
  expect_type(d$y0, "double")
  expect_type(d$y0_ul, "double")
})

test_that("flum computes fluglm model", {
  d <- flum(fludta, model="fluglm", 
            outc=perc_fludeaths, time=yrweek_dt,
            viral='prop_flupos')
  
  #load data
  dnames <- c("year", "week", "fludeaths", "alldeaths", 
              "perc_fludeaths", "yrweek_dt", "prop_flupos",
              "fit", "upper", 'lower')
  
  #Tests
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 261L)
  expect_that(dnames, equals(names(d)))
  expect_type(d$fit, "double")
  expect_type(d$upper, "double")
  expect_type(d$lower, "double")
})
  