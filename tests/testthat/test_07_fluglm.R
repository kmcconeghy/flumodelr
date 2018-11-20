library(flumodelr)
flu_dta <- flumodelr::fludta

context("fluglm")

test_that("test fluglm function - season ", {
  
  d <- fluglm(fludta, outc = fludeaths, season=T, time = yrweek_dt)  
  nms <- c('epi', 'fit', 'upper', 'lower')
  
  #Tests
  expect_s3_class(d, "data.frame")
  expect_true(all(nms %in% names(d)))
  expect_type(d$fit, "double")
  expect_type(d$upper, "double")  
  expect_type(d$lower, "double")
  expect_gte(min(d$fit), 0)
  expect_gte(min(d$upper), 0)
  expect_true(all(!is.na(d$fit)))
})

test_that("test fluglm function - viral ", {
  
  d <- fluglm(fludta, outc = fludeaths, viral='prop_flupos', time = yrweek_dt)  
  nms <- c('fit', 'upper', 'lower')
  
  #Tests
  expect_s3_class(d, "data.frame")
  expect_true(all(nms %in% names(d)))
  expect_type(d$fit, "double")
  expect_type(d$upper, "double")  
  expect_type(d$lower, "double")
  expect_gte(min(d$fit), 0)
  expect_gte(min(d$upper), 0)
  expect_true(all(!is.na(d$fit)))
})

test_that("test fluglm function - neg binomial ", {
  
  d <- fluglm(fludta, outc = fludeaths, viral='prop_flupos', time = yrweek_dt,
              glmnb=T)  
  nms <- c('fit', 'upper', 'lower')
  
  #Tests
  expect_s3_class(d, "data.frame")
  expect_true(all(nms %in% names(d)))
  expect_type(d$fit, "double")
  expect_type(d$upper, "double")  
  expect_type(d$lower, "double")
  expect_gte(min(d$fit), 0)
  expect_gte(min(d$upper), 0)
  expect_true(all(!is.na(d$fit)))
})

test_that("test fluglm function - poisson", {
  d <- fluglm(fludta, 
              outc = fludeaths, 
              time = yrweek_dt, 
              season =T,
              offset = log(alldeaths),
              family = poisson) #nonsense variabl for testing
  
  nms <- c('fit', 'upper', 'lower')
  
  #Tests
  expect_s3_class(d, "data.frame")
  expect_true(all(nms %in% names(d)))
  expect_type(d$fit, "double")
  expect_type(d$upper, "double")  
  expect_type(d$lower, "double")
  expect_gte(min(d$fit), 0)
  expect_gte(min(d$upper), 0)
  expect_true(all(!is.na(d$fit)))
})
  