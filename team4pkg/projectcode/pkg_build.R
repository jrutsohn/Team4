setwd('/Users/jrutsohn/Documents/GitHub/')
library(usethis)
library(roxygen2)
library(devtools)
library(testthat)


create_package("team4pkg", rstudio=F)
setwd("/Users/jrutsohn/Documents/GitHub/team4pkg")
roxygen2::roxygenise()
usethis::use_testthat()
usethis::use_package("ggplot2")
usethis::use_package("caret")
usethis::use_package("data.table")
usethis::use_package("dplyr")
usethis::use_package("magrittr")
usethis::use_package("naniar")
usethis::use_package("missForest")
usethis::use_package("mice")
#' @import ggplot2
#' @import caret
#' @import data.table
#' @import dplyr
#' @import naniar
#' @import missForest
#' @import magrittr
#' @import mice
use_test("team4pkg")

setwd("/Users/jrutsohn/Documents/GitHub/")
build("team4pkg")

load_all("team4pkg")

