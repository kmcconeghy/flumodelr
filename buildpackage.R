library('devtools')

setwd("~/GitHub/flumodelr")

## Build
devtools::document()
devtools::check()
devtools::build()
devtools::test()
pkgdown::build_site()


