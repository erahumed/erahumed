test_that("data.frame has no NAs", {
  expect_no_error(na.fail(albufera_weather))
})

test_that("date domain is an interval", {
  expect_true(all( diff(albufera_weather$date) == 1) )
})

test_that("temperatures are ordered as expected (min <= ave <= max)", {
  expect_true(all(
    albufera_weather$temperature_min <= albufera_weather$temperature_ave
    ))
  expect_true(all(
    albufera_weather$temperature_ave <= albufera_weather$temperature_max
  ))
})

test_that("temperatures are in a reasonable range", {
  expect_true(all(albufera_weather$temperature_min >= -30))
  expect_true(all(albufera_weather$temperature_max <= 60))
})

test_that("Precipitation and evapotranspiration values are always positive", {
  expect_true(all( albufera_weather$precipitation_mm >= 0 ))
  expect_true(all( albufera_weather$evapotranspiration_mm >= 0 ))
})
