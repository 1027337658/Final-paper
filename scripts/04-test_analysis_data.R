#### Preamble ####
# Purpose: Tests of cleaned Trackinginjustice Website Data
# Author: Yuanting Han
# Date:  29 November 2024
# Contact: q2900725357@gmail.com 
# License: MIT
# Pre-requisites: 
  # - The `tidyverse` package must be installed and loaded 
  # - The `testthat` package must be installed and loaded
  # - The `arrow` package must be installed and loaded
  # - 03-clean_data.R must have been run
# Any other information needed?  Make sure in the  `Injustice` rproj


#### Workspace setup ####
library(tidyverse)
library(testthat)
library(arrow)

analysis_data <- read_parquet("data/02-analysis_data/analysis_data.parquet")


#### Test data ####
# Test that the dataset has 301 rows - there are 301 cases 
test_that("dataset has 301 rows", {
  expect_equal(nrow(analysis_data), 301)
})

# Test that the dataset has 9 columns
test_that("dataset has 9 columns", {
  expect_equal(ncol(analysis_data), 9)
})

# Test that the 'age' column is double type
test_that("'age' is double", {
  expect_type(analysis_data$age, "double")
})

# Test that the 'gender' column is double type
test_that("'gender' is double", {
  expect_type(analysis_data$gender, "double")
})

# Test that the 'gunshot' column is double type
test_that("'gunshot' is double", {
  expect_type(analysis_data$gunshot, "double")
})

# Test that the 'year' column is double type
test_that("'year' is double", {
  expect_type(analysis_data$year, "double")
})

# Test that the 'race' column is character type
test_that("'race' is character", {
  expect_type(analysis_data$race, "character")
})

# Test that the 'province' column is character type
test_that("'province' is character", {
  expect_type(analysis_data$province, "character")
})

# Test that the 'police' column is character type
test_that("'police' is character", {
  expect_type(analysis_data$police, "character")
})

# Test that the 'period' column is character type
test_that("'period' is character", {
  expect_type(analysis_data$period, "character")
})

# Test that the 'prname' column is character type
test_that("'prname' is character", {
  expect_type(analysis_data$prname, "character")
})

# Test that the 'gender' has 2 unique values
test_that("'gender' has 2 unique values", {
  expect_length(unique(analysis_data$gender), 2)
})

# Test that the 'race' has 2 unique values
test_that("'race' has 2 unique values", {
  expect_length(unique(analysis_data$race), 2)
})

# Test that the 'gunshot' has 2 unique values
test_that("'gunshot' has 2 unique values", {
  expect_length(unique(analysis_data$gunshot), 2)
})

# Test that there are no missing values in the dataset
test_that("no missing values in dataset", {
  expect_true(all(!is.na(analysis_data)))
})


# Test that 'prname' contains only valid Canada province names
valid_provinces <- c('Ontario','Newfoundland and Labrador',
'British Columbia',
'Saskatchewan','Quebec','Alberta',
'New Brunswick',
'Nova Scotia','Manitoba',
'Prince Edward Island')

test_that("'prname' contains valid Canada province names", {
  expect_true(all(analysis_data$prname %in% valid_provinces))
})

# Test that there are no empty strings in character type columns
test_that("no empty strings in 'race', 'province', 'police', 'period' or 'prname' columns", {
  expect_false(any(analysis_data$race == "" | analysis_data$province == "" | analysis_data$police == "" | analysis_data$period == "" | analysis_data$prname == ""))
})


