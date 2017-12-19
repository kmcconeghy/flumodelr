library('devtools')

setwd("~/GitHub/flumodelr")

first_time <- F

if (first_time==T) {
  #Set up  Folders 
    ## Data Folders
      devtools::use_data_raw()
      source('~\\GitHub\\flumodelr\\data-raw\\RawDataToRData.R')
  
    ##Set-up Test Folder
      devtools::use_testthat()
    
    ##Set-up vignettes
      devtools::use_vignette("00-introduction")
      devtools::use_vignette("01-data")
      devtools::use_vignette("02-serfling")
  }

## Build
devtools::document()
devtools::check()
devtools::build()
devtools::test()
pkgdown::build_site()

