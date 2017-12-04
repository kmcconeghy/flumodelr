## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(dplyr)
library(ggplot2)

## ------------------------------------------------------------------------
cdc122 <- (flumodelr::cdc122city)  
dim(cdc122)
cdc122

## ------------------------------------------------------------------------
nrevss <- (flumodelr::nrevss)  
dim(nrevss)
nrevss

## ------------------------------------------------------------------------
ilinet <- (flumodelr::ilinet)  
dim(ilinet)
ilinet

