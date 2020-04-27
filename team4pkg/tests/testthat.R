library(testthat)
library(team4pkg)

test_check("team4pkg")

test_that("imputed dataset has no missing", {
    expect_equal(0,sum(is.na(imputedData)))
})

test_that("n >> p for solving", {
    expect_gt(dim(X)[1],dim(X)[2])
})    

test_that("conformable matrix multiplication", {
    expect_equal(dim(d2)[2],dim(d1)[1])
})

test_that("multiplication works", {
    expect_equal(2*2,4)
})

test_that("addition works", {
    expect_equal(2+2,4)
})

expect_warning(dim(X)[2] != length(param), "Data and parameter dimensions do not match")

expect_warning(!is.vector(param), "Input parameters need to be a vector")

expect_warning(dim(param)[2] != 1), "Input parameters needs to be length 1")
