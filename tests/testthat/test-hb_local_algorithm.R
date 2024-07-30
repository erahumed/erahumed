test_that("make_lhb_df_list succeds if inputs can form a data.frame", {
  expect_no_condition(
    make_lhb_df_list(
      ideal_height_cm = rep(10, 10),
      irrigation = rep(TRUE, 10),
      draining = rep(TRUE, 10),
      petp_cm = rnorm(10),
      area_m2 = runif(10, 1e6, 1e7),
      capacity_m3_s = runif(10, 1, 2),
      date = c(
        rep(as.Date("2020-01-01"), 5),
        rep(as.Date("2020-01-02"), 5)
      ),
      cluster_id = letters[1:10]
    )
  )
})

test_that("make_lhb_df_list throws error if inputs cannot form a data.frame", {
  expect_error(
    make_lhb_df_list(
      ideal_height_cm = rep(10, 10),
      irrigation = rep(TRUE, 10),
      draining = rep(TRUE, 10),
      petp_cm = rnorm(3),
      area_m2 = runif(10, 1e6, 1e7),
      capacity_m3_s = runif(10, 1, 2),
      date = c(
        rep(as.Date("2020-01-01"), 5),
        rep(as.Date("2020-01-02"), 5)
      ),
      cluster_id = letters[1:10]
    )
  )

  skip("Corner case if vector with mismatched size can be recycled")
  expect_error(
    make_lhb_df_list(
      ideal_height_cm = rep(10, 10),
      irrigation = rep(TRUE, 10),
      draining = rep(TRUE, 10),
      petp_cm = rnorm(2),
      area_m2 = runif(10, 1e6, 1e7),
      capacity_m3_s = runif(10, 1, 2),
      date = c(
        rep(as.Date("2020-01-01"), 5),
        rep(as.Date("2020-01-02"), 5)
      ),
      cluster_id = letters[1:10]
    )
  )

})

test_that("make_lhb_df_list throws error if date or cluster_id are missing", {
  expect_error(
    make_lhb_df_list(
      ideal_height_cm = rep(10, 10),
      irrigation = rep(TRUE, 10),
      draining = rep(TRUE, 10),
      petp_cm = rnorm(3),
      area_m2 = runif(10, 1e6, 1e7),
      capacity_m3_s = runif(10, 1, 2),
      # date = c(
      #   rep(as.Date("2020-01-01"), 5),
      #   rep(as.Date("2020-01-02"), 5)
      # ),
      cluster_id = letters[1:10]
    )
  )

  expect_error(
    make_lhb_df_list(
      ideal_height_cm = rep(10, 10),
      irrigation = rep(TRUE, 10),
      draining = rep(TRUE, 10),
      petp_cm = rnorm(3),
      area_m2 = runif(10, 1e6, 1e7),
      capacity_m3_s = runif(10, 1, 2),
      date = c(
        rep(as.Date("2020-01-01"), 5),
        rep(as.Date("2020-01-02"), 5)
      ),
      # cluster_id = letters[1:10]
    )
  )

})

test_that("make_lhb_df_list returns a list of dataframes", {
  res <- make_lhb_df_list(
      ideal_height_cm = rep(10, 10),
      irrigation = rep(TRUE, 10),
      draining = rep(TRUE, 10),
      petp_cm = rnorm(10),
      area_m2 = runif(10, 1e6, 1e7),
      capacity_m3_s = runif(10, 1, 2),
      date = c(
        rep(as.Date("2020-01-01"), 5),
        rep(as.Date("2020-01-02"), 5)
      ),
      cluster_id = letters[1:10]
    )

  expect_type(res, "list")
  lapply(res, expect_s3_class, "data.frame")
})

test_that("make_lhb_df_list returned dataframes contain the input columns", {
  args <- list(
    ideal_height_cm = rep(10, 10),
    irrigation = rep(TRUE, 10),
    draining = rep(TRUE, 10),
    petp_cm = rnorm(10),
    area_m2 = runif(10, 1e6, 1e7),
    capacity_m3_s = runif(10, 1, 2),
    date = c(
      rep(as.Date("2020-01-01"), 5),
      rep(as.Date("2020-01-02"), 5)
    ),
    cluster_id = letters[1:10],
    extra_column_1 = 1:10,
    extra_column_2 = sample(10),
    extra_column_3 = rpois(10, 1)
  )

  res <- do.call(make_lhb_df_list, args)

  expect_contains(names(res[[1]]), names(args))

})

test_that("make_lhb_df_list returned dataframes are grouped by date", {
  args <- list(
    ideal_height_cm = rep(10, 10),
    irrigation = rep(TRUE, 10),
    draining = rep(TRUE, 10),
    petp_cm = rnorm(10),
    area_m2 = runif(10, 1e6, 1e7),
    capacity_m3_s = runif(10, 1, 2),
    date =rep(
      seq.Date(from = as.Date("2020-01-01"),
               to = as.Date("2020-01-05"),
               by = "day"),
      2),
    cluster_id = letters[1:10],
    extra_column_1 = 1:10,
    extra_column_2 = sample(10),
    extra_column_3 = rpois(10, 1)
  )

  res <- do.call(make_lhb_df_list, args)

  unique_dates <- lapply(res, \(df) unique(df$date))

  lapply(unique_dates, expect_length, 1)

})

test_that("make_lhb_df_list returned dataframes are sorted by date", {
  args <- list(
    ideal_height_cm = rep(10, 10),
    irrigation = rep(TRUE, 10),
    draining = rep(TRUE, 10),
    petp_cm = rnorm(10),
    area_m2 = runif(10, 1e6, 1e7),
    capacity_m3_s = runif(10, 1, 2),
    date =rep(
      seq.Date(from = as.Date("2020-01-01"),
               to = as.Date("2020-01-05"),
               by = "day"),
      2),
    cluster_id = letters[1:10],
    extra_column_1 = 1:10,
    extra_column_2 = sample(10),
    extra_column_3 = rpois(10, 1)
  )

  res <- do.call(make_lhb_df_list, args)

  unique_dates <- sapply(res, \(df) unique(df$date))

  expect_identical(unique_dates, sort(unique_dates))

})
