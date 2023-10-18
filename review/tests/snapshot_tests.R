library(testthat)
library(withr)
library(rprojroot)

local_edition(3)

test_that("01_headline_figures text is as expected", {
  with_dir(find_package_root_file(), {
    source("review/scripts/word_to_md.R")
    
    expect_snapshot_file("review/temp/01_headline_figures.md")
  })
})