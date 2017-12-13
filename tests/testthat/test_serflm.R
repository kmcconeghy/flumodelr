suppressMessages({
  library(dplyr)
  library(lubridate)
  library(tibble)})

context("serflm() function")


test_that("serflm computes correct estimates", {
  
  d <- flumodelr::flu_ex %>%
    mutate(epi = if_else(month(yrweek_dt)>=10 | month(yrweek_dt)<=5, T, F))  
  
  
  #Stored as "correct" values
  firstFivePred <- c(6.325176, 6.415187, 6.507871, 6.601823, 6.695621)
  lastFivePred <- c(4.977252, 5.025265, 5.082895, 5.149250, 5.223308)
  
  d_pred <- serflm(data=d, outc="fludeaths", epi="epi", time="yrweek_dt")

  #load data
  dnames <- c("year", "week", "fludeaths", "yrweek_dt", "prop_flupos",
              "epi", "pred_y0", "pred_y0_serf")
  
  #Tests
  expect_s3_class(d_pred, "data.frame")
  expect_that(dnames, equals(names(d_pred)))
  expect_type(d_pred$pred_y0, "double")
  expect_type(d_pred$pred_y0, "double")
  expect_type(d_pred$pred_y0_serf, "double")
  expect_type(d_pred$pred_y0_serf, "double")
  expect_equal(d_pred[[7]][1:5], firstFivePred, tolerance=1.0e-07)
  expect_equal(d_pred[[7]][255:259], lastFivePred, tolerance=1.0e-07)
  
})