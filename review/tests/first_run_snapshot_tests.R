library(testthat)
library(withr)
library(rprojroot)

local_edition(3)

test_that("01_headline_figures text is as expected", {
  with_dir(find_package_root_file(), {
    expect_snapshot_file("inst/markdown/01_headline_figures.md")
  })
})