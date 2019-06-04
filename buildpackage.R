
setwd("~/GitHub/flumodelr")

## Build
devtools::document()
pkgbuild::build()
devtools::test()
devtools::check()


pkgdown::build_site()


