library(tidyverse, quietly = T, warn.conflicts = F)
library(lubridate, quietly = T, warn.conflicts = F)
library(scales, quietly = T, warn.conflicts = F)

context("fluplot")


test_that("fluplot generates plot", {
  d <- flumodelr::fludta 
  d_pred <- fluserf(data=d, outc=perc_fludeaths, time=yrweek_dt)
  g <- fluplot(d_pred, xvar=yrweek_dt, perc_fludeaths, y0, y0_ul,
               ylab="% Deaths due to P&I", title="Serfling Model")
  
  #Tests
  expect_s3_class(g, "gg")
})