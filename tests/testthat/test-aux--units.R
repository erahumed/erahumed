test_that("cm_day_to_m3_s() returns correct value in simple case", {
  input_cm <- 1 * 100  # 1 m3 of water in 1 day
  area_m2 <- 1

  expected <- 1 / s_per_day()
  expect_equal(cm_day_to_m3_s(input_cm, area_m2), expected)
})

test_that("m3_s_to_cm_day() returns correct value in simple case", {
  input_m3_s <- 1  # 1 m3/s of water during 1 day
  area_m2 <- 1

  expected <- 100 * s_per_day()
  expect_equal(m3_s_to_cm_day(input_m3_s, area_m2), expected)
})
