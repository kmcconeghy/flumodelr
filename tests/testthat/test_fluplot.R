library(tidyverse, quietly = T, warn.conflicts = F)
library(lubridate, quietly = T, warn.conflicts = F)
library(scales, quietly = T, warn.conflicts = F)

context("fluplot")


test_that("fluplot generates plot", {
  d <- flumodelr::flu_ex 
  d_pred <- serflm(data=d, outc=fludeaths, time=yrweek_dt)
  g <-fluplot(d_pred, xvar=yrweek_dt, fludeaths, y0, y0_ul,
              ylab="Mortality per 100,000", title="Serfling Model")
  
  #Tests
  expect_s3_class(g, "gg")
})