context("Incidence Rate Difference")

test_that("ird() function executes", {
  
  #load data
  d <- flumodelr::fludta
  d_fit <- ird(data=fludta, 
               outc = perc_fludeaths, 
               viral=prop_flupos, 
               time=yrweek_dt)

  fitvarnms <- c('high', 'fluseason')
  
  #Tests
  expect_s3_class(d_fit, "data.frame")
  expect_type(d_fit$high, "logical")
  expect_type(d_fit$fluseason, "logical")
})

test_that("rb() function executes", {
  #load data
  d_fit <- ird(data=flumodelr::fludta, 
               outc = perc_fludeaths, 
               viral=prop_flupos, 
               time=yrweek_dt)
  d_fit <- rb(d_fit, perc_fludeaths)
  dnames <- c('season', 'high_act', 'perc_fludeaths_fluseason',
              'perc_fludeaths_viral_act')
  #Tests
  expect_s3_class(d_fit, "data.frame")
  expect_that(dnames, equals(names(d_fit)))
  expect_type(d_fit$season, "double")
  expect_type(d_fit$high_act, "logical")
  expect_type(d_fit$perc_fludeaths_fluseason, "double")
  expect_type(d_fit$perc_fludeaths_viral_act, "double")
})