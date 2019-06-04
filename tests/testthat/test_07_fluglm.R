library(flumodelr)

context("fluglm")

test_that("test fluglm function - season ", {
  
  d <- fluglm(fludta, outc = fludeaths, season=epi, time = week_in_order)  
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

test_that("test fluglm function - viral ", {
d <-  fludta %>%
  dplyr::filter(!is.na(prop_flupos)) %>%
  mutate(week_in_order = row_number()) %>%
  fluglm(., outc = fludeaths, viral='prop_flupos', time = week_in_order)  
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

test_that("test fluglm function - neg binomial ", {
  
d <-  fludta %>%
    dplyr::filter(!is.na(prop_flupos)) %>%
    mutate(week_in_order = row_number()) %>%
    fluglm(., outc = fludeaths, viral='prop_flupos', time = week_in_order,
              glmnb=T)  
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

test_that("test fluglm function - poisson", {
d <-  fludta %>%
    dplyr::filter(!is.na(prop_flupos)) %>%
    mutate(week_in_order = row_number()) %>%
    fluglm(., 
              outc = fludeaths, 
              time = week_in_order, 
              season =epi,
              offset = log(alldeaths),
              family = poisson) #nonsense variabl for testing
  
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
  