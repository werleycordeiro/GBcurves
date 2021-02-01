library(testthat)
library(GBcurves)

init <- "2020-05-10"
fin <- "2020-05-17"
mty <- c(3,6,12,120,360)
ctry <- "BR"

df.out <- yields(init = init, fin = fin, mty = mty, ctry = "BR")

test_that(desc = 'Test function',{
  expect_true(nrow(df.out)>0)
  } )

df.out <- yields(init = init, fin = fin, mty = mty, ctry = "CN")

test_that(desc = 'Test function',{
  expect_true(nrow(df.out)>0)
} )

df.out <- yields(init = init, fin = fin, mty = mty, ctry = "RU")

test_that(desc = 'Test function',{
  expect_true(nrow(df.out)>0)
} )

