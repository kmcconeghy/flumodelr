library(tidyverse, quietly = T, warn.conflicts = F)
library(lubridate, quietly = T, warn.conflicts = F)

d <- flumodelr::fludta 
dnames <- c("year", "week", "fludeaths", "alldeaths", 
            "perc_fludeaths", "yrweek_dt", "prop_flupos",
            "fit", "upper", "lower")

context("fluglm")

test_that("Usage example works", {
  flu_fit <- fluglm(d, 
                    season=T, 
                    outc = fludeaths, 
                    time = yrweek_dt) 
  
  expect_s3_class(flu_fit, "data.frame")
  expect_that(dnames, equals(names(flu_fit)))
  expect_equal(nrow(flu_fit), 261L)
  expect_type(flu_fit$fit, "double")
  expect_type(flu_fit$upper, "double")
  expect_type(flu_fit$lower, "double")
  
})

test_that("fluglm gaussian computes", {
  flu_fit <- fluglm(d, 
                   outc = perc_fludeaths, 
                   season=T,
                   time = yrweek_dt, 
                   family = gaussian) 
  #Tests
  expect_s3_class(flu_fit, "data.frame")
  expect_that(dnames, equals(names(flu_fit)))
  expect_equal(nrow(flu_fit), 261L)
  expect_type(flu_fit$fit, "double")
  expect_type(flu_fit$upper, "double")
  expect_type(flu_fit$lower, "double")
})

test_that("fluglm poisson computes", {
  flu_fit <- fluglm(d, 
                   outc = fludeaths, 
                   time = yrweek_dt, 
                   season=T,
                   family = poisson,
                   offset = log(alldeaths)) 
  
  #Tests
  expect_s3_class(flu_fit, "data.frame")
  expect_equal(nrow(flu_fit), 261L)
  expect_that(dnames, equals(names(flu_fit)))
  expect_type(flu_fit$fit, "double")
  expect_type(flu_fit$upper, "double")
  expect_type(flu_fit$lower, "double")
})

test_that("fluglm viral computes", {
  flu_fit <- fluglm(d, 
                   outc = fludeaths, 
                   time = yrweek_dt, 
                   family = poisson,
                   offset = log(alldeaths),
                   viral="prop_flupos") 
  
  #Tests
  expect_s3_class(flu_fit, "data.frame")
  expect_equal(nrow(flu_fit), 261L)
  expect_that(dnames, equals(names(flu_fit)))
  expect_type(flu_fit$fit, "double")
  expect_type(flu_fit$upper, "double")
  expect_type(flu_fit$lower, "double")
})