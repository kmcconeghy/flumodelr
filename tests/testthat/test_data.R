context("Package Data Files")

test_that("flu_ex properly formatted", {
  #load data
  d <- flumodelr::flu_ex
  dnames <- c("year", "week", "fludeaths", "alldeaths", 
              "perc_fludeaths", "yrweek_dt", "prop_flupos")
  
  #Tests
  expect_s3_class(d, "data.frame")
  expect_that(dnames, equals(names(d)))
  expect_type(d$year, "integer")
  expect_type(d$week, "integer")
  expect_type(d$fludeaths, "integer")
  expect_type(d$alldeaths, "integer")
  expect_type(d$perc_fludeaths, "double")
  expect_s3_class(d$yrweek_dt, "Date")
  expect_type(d$prop_flupos, "double")
  expect_that(min(flu_ex$year), equals(2010))
  expect_that(max(flu_ex$year), equals(2015))
  expect_that(min(flu_ex$week), equals(1))
  expect_that(max(flu_ex$week), equals(53))
})