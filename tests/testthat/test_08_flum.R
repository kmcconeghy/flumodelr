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
              "week_in_order", "epi", "season", "high", "fluseason")
  
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
              "week_in_order", "epi", "y0", "y0_ul")
  
  #Tests
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 261L)
  expect_that(dnames, equals(names(d)))
  expect_type(d$y0, "double")
  expect_type(d$y0_ul, "double")
})

test_that("flum computes fluglm model", {
  d <- flum(fludta, model="fluglm", 
            outc=perc_fludeaths, time=week_in_order,
            viral='prop_flupos')
  
  nms <- c('flu_pred', 'flu_pred_upr', 'flu_pred_lwr')
  
  #Tests
  expect_s3_class(d, "data.frame")
  expect_true(all(nms %in% names(d)))
  expect_type(d$flu_pred, "double")
  expect_type(d$flu_pred_upr, "double")  
  expect_type(d$flu_pred_lwr, "double")
  expect_type(d$flu_base, "double")
  expect_type(d$flu_base_upr, "double")  
  expect_type(d$flu_base_lwr, "double")
  expect_gte(min(d$flu_pred), 0)
  expect_gte(min(d$flu_base), 0)
  expect_true(all(!is.na(d$flu_pred)))
})
  