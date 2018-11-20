library(flumodelr, quietly = T, warn.conflicts = F)

context("fluglm")

test_that("mk_flu_form - with outc, offset, poly", {
  flu_form <- mk_flu_form(outc =y0, 
                          offset =log(persons), 
                          poly=T)
  
  expect_type(flu_form, 'character')
  expect_that(str_detect(flu_form, '^y0'), equals(T))
  expect_that(str_detect(flu_form, 'offset'), equals(T))
  expect_that(str_detect(flu_form, 'I'), equals(T))
  })

test_that("mk_flu_form - no offset, poly", {
  flu_form <- mk_flu_form(outc =y0, poly=F)
  
  expect_type(flu_form, 'character')
  expect_that(str_detect(flu_form, '^y0'), equals(T))
  expect_that(str_detect(flu_form, 'offset'), equals(F))
  expect_that(str_detect(flu_form, 'I'), equals(F))
})

test_that("model formula accepts formula..", {
  
  my_form <- as.formula(y ~ a + b + c)
  
  flu_form <- mk_flu_form(model_form = my_form)
  
  expect_type(flu_form, 'language')
})