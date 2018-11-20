library('devtools')

setwd("~/GitHub/flumodelr")

## Build
devtools::document()
devtools::build()
devtools::test()
devtools::check()


pkgdown::build_site()


