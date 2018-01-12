library(tidyverse, quietly = T, warn.conflicts = F)

context("fludiff")


test_that("fludiff computes", {
  
  d <- flumodelr::fludta 
  d_pred <- fluserf(data=d, outc=perc_fludeaths, time=yrweek_dt)
  d_diff <- fludiff(d_pred, obsvar = perc_fludeaths, fitvar = y0_ul)

  #load data
  dnames <- c("year", "week", "fludeaths", "alldeaths", 
              "perc_fludeaths", "yrweek_dt", "prop_flupos",
              "epi", "y0", "y0_ul", "y_diff")
  
  #Tests
  expect_s3_class(d_diff, "data.frame")
  expect_that(dnames, equals(names(d_diff)))
  expect_type(d_diff$y_diff, "double")
})