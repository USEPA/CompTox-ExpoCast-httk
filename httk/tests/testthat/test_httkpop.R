#Tests for httkpop population-generator

library(httk)

test_that("basic httkpop_generate() function calls run without errors",{
  expect_error(httkpop_generate(method = "d",
                                  nsamp = 10),
               NA)
  expect_error(httkpop_generate(method = "v",
                                nsamp = 10),
               NA)
}
          )

test_that("agelim_years arguments work in httkpop_generate()", {
  min_ageyears <- 18
  max_ageyears <- 65
  foo_d <- httkpop_generate(method = "d",
                            nsamp = 10,
                            agelim_years = c(min_ageyears,
                                             max_ageyears))
  foo_v <- httkpop_generate(method = "v",
                            nsamp = 10,
                            agelim_years = c(min_ageyears,
                                             max_ageyears))
  expect_gte(foo_d[, min(age_years)], min_ageyears)
  expect_lte(foo_d[, max(age_years)], max_ageyears)
})

test_that("agelim_months arguments work in httkpop_generate()", {
  min_agemos <- 18*12
  max_agemos <- 65*12+11
  foo_d <- httkpop_generate(method = "d",
                            nsamp = 10,
                            agelim_months = c(min_agemos,
                                             max_agemos))
  foo_v <- httkpop_generate(method = "v",
                            nsamp = 10,
                            agelim_months = c(min_agemos,
                                             max_agemos))
  expect_gte(foo_d[, min(age_months)], min_agemos)
  expect_lte(foo_d[, max(age_months)], max_agemos)
})

test_that("")