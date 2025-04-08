test_that("data.frame has no NAs", {
  expect_no_error(na.fail(albufera_management))
})

test_that("Data-set has the expected cardinality", {
  n_cat_tancat <- 2
  n_varieties <- 3
  n_days <- 366

  expect_true(nrow(albufera_management) == n_cat_tancat * n_varieties * n_days)
})

test_that("Date domain covers every day of a regular (non-leap) year", {
  days <- seq.Date(from = as.Date("2001-01-01"),
                   to = as.Date("2001-12-31"),
                   by = "day") |>
    as.POSIXlt()
  dates_df <- albufera_management[, c("mm", "dd")] |> dplyr::distinct()

  test_df <- data.frame(mm = get_mm(days), dd = get_dd(days)) |>
    merge(dates_df, by = c("mm", "dd"))

  expect_true( nrow(test_df) == length(days) )
})

test_that("Date domain covers 29th of February (for a leap year)", {
  test_df <- albufera_management[, c("mm", "dd")] |>
    dplyr::distinct() |>
    (\(.) .[.$mm == 2 & .$dd == 29, ])()

  expect_true( nrow(test_df) == 1 )
})

test_that("'sowing' and 'harvesting' have a single TRUE per (tancat,variety)", {
  test_df <- albufera_management |>
    dplyr::summarise(
      n_sowing = sum(sowing),
      n_harvesting = sum(harvesting),
      .by = c("tancat", "variety"))

  expect_true(all( test_df$n_sowing == 1 ))
  expect_true(all( test_df$n_harvesting == 1 ))

})

test_that("seed_day == 0 iff sowing is TRUE", {
  idx_seed_day <- which(albufera_management$seed_day == 0)
  idx_sowing <- which(albufera_management$sowing)

  expect_identical(idx_seed_day, idx_sowing)
})

test_that("ideal_height_eod_cm is always positive", {
  expect_true(all( albufera_management$ideal_height_eod_cm >= 0 ))

})
