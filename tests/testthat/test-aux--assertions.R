test_that("assert_length_one(): succeeds if length = 1", {
  expect_true(assert_length_one(1))
})

test_that("assert_length_one(): throws if length > 1", {
  expect_error(assert_length_one(1:10))
})



test_that("assert_integer_vector(): detects non-integer numbers", {
  expect_error(assert_integer_vector(0.5))
})



test_that("assert_atomic(): detects non-atomic objects", {
  expect_error(assert_atomic(list()))
})



test_that("assert_function(): detects non-function objects", {
  expect_error(assert_function(list()))
})



test_that("assert_data.frame(): detects wrong column types", {
  df <- template <- albufera_weather
  df$date <- as.character(df$date)

  expect_error(assert_data.frame(df, template = template))
})

test_that("assert_data.frame(): skips type checks if !check_types", {
  df <- template <- albufera_weather
  df$date <- as.character(df$date)

  expect_no_error(assert_data.frame(df, template = template, check_types = F))
})

test_that("assert_data.frame(): skips extra column if extends = TRUE", {
  df <- template <- albufera_weather
  df$date_char <- as.character(df$date)

  expect_no_error(assert_data.frame(df, template = template, extends = TRUE))
})

test_that("assert_data.frame(): detect extra column if !extends", {
  df <- template <- albufera_weather
  df$date_char <- as.character(df$date)

  expect_error(assert_data.frame(df, template = template, extends = F))
})



test_that("assert_erahumed_simulation(): detect wrong type objects", {
  expect_error(assert_erahumed_simulation(list()))
})


