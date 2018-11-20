context("Package Data Files")

test_that("fludta properly formatted", {
  #load data
  d <- flumodelr::fludta
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
  expect_that(min(d$year), equals(2010))
  expect_that(max(d$year), equals(2015))
  expect_that(min(d$week), equals(1))
  expect_that(max(d$week), equals(53))
})

test_that("CDC 122-city data properly formatted", {
  #load data
  d <- flumodelr::cdc122city
  dnames <- c('region', 'state', 'city', 'year', 'week',
              'deaths_pnaflu', 'deaths_allcause', 'deaths_65older')
  
  #Tests
  expect_s3_class(d, "data.frame")
  expect_that(dnames, equals(names(d)))
  expect_type(d$year, "integer")
  expect_type(d$week, "integer")
  expect_type(d$deaths_pnaflu, "integer")
  expect_type(d$deaths_allcause, "integer")
  expect_type(d$deaths_65older, "integer")
  expect_type(d$city, "character")
  expect_that(min(d$year), equals(1962))
  expect_that(max(d$year), equals(2016))
  expect_that(min(d$week), equals(1))
  expect_that(max(d$week), equals(53))
})
test_that("ILI-Net data valid", {
  #load data
  d <- flumodelr::ilinet
  dnames <- c('state', 'year', 'week', 'ili_perc', 'ili_tot', 
              'providers', 'patients')
  
  #Tests
  expect_s3_class(d, "data.frame")
  expect_that(dnames, equals(names(d)))
  expect_type(d$year, "integer")
  expect_type(d$week, "integer")
  expect_type(d$ili_perc, "double")
  expect_type(d$ili_tot, "integer")
  expect_type(d$providers, "integer")
  expect_type(d$patients, "integer")
  expect_type(d$state, "character")
  expect_that(min(d$year), equals(2010))
  expect_that(max(d$year), equals(2017))
  expect_that(min(d$week), equals(1))
  expect_that(max(d$week), equals(53))
})
test_that("NREVSS data valid", {
  #load data
  d <- flumodelr::nrevss
  dnames <- c("state", "year", "week", "spec_tot", "spec_pos", 
              "type_A_h1n1", "type_A_h1", "type_A_h3", "type_A_np", 
              "type_A_us", "type_B", "type_A_h3n2")
  
  #Tests
  expect_s3_class(d, "data.frame")
  expect_that(dnames, equals(names(d)))
  expect_type(d$year, "integer")
  expect_type(d$week, "integer")
  expect_type(d$spec_tot, "integer")
  expect_type(d$spec_pos, "double")
  expect_type(d$type_A_h1n1, "integer")
  expect_type(d$type_A_h1, "integer")
  expect_type(d$type_A_h3, "integer")
  expect_type(d$type_A_np, "integer")
  expect_type(d$type_A_us, "integer")
  expect_type(d$type_B, "integer")
  expect_type(d$type_A_h3n2, "integer")
  expect_type(d$state, "character")
  expect_that(min(d$year), equals(2010))
  expect_that(max(d$year), equals(2015))
  expect_that(min(d$week), equals(1))
  expect_that(max(d$week), equals(53))
})