# test-customer-segmentation.R

library(assertthat)
library(testthat)

#' Test that refactored code generates the same data structure as the original code
testthat::test_that("replicated_df", {
  orig_df <- generateDataOrig()
  new_df <- generateData()
  
  assertthat::assert_that(class(orig_df) == class(new_df))
  
  # all cell comparisons should be TRUE
  comparison_res <- (orig_df == new_df)
  length <- length(comparison_res)
  total <- sum(comparison_res)
  assertthat::assert_that(length == total)
  
  orig_names <- names(orig_df)
  new_names <- names(new_df)
  assertthat::assert_that(length(orig_names) == sum(orig_names == new_names))
  assertthat::assert_that(1 == 1)
  
})

testthat::test_that("intuition is correct",{
  sim_df <- generateData()
  assertthat::assert_that(nrow(sim_df)==sum(DEF_SEG_SIZES))
  
})