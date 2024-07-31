test_that("make_lhb_df_list() succeds if inputs can form a data.frame", {
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

test_that("make_lhb_df_list() throws if inputs cannot form a data.frame", {
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

  skip("make_lhb_df_list(): corner case if wrong size input can be recycled")
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

test_that("make_lhb_df_list() throws if date or cluster_id are missing", {
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

test_that("make_lhb_df_list() returns a list of dataframes", {
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

test_that("make_lhb_df_list() returned dfs contain the input columns", {
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

test_that("make_lhb_df_list() returned dfs are grouped by date", {
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

test_that("make_lhb_df_list() returned dfs are sorted by date", {
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



test_that("compute_ideal_diff_flow_cm() returns a (properly) named list", {
  res <- compute_ideal_diff_flow_cm(
    ideal_height_cm = 10, real_height_cm_lag = 8, petp_cm = 1
  )

  expect_type(res, "list")
  expect_identical(names(res), "ideal_diff_flow_cm")
})

test_that("compute_ideal_diff_flow_cm() returns the correct structure", {
  set.seed(840)
  n <- 10

  res <- compute_ideal_diff_flow_cm(
    ideal_height_cm = runif(n, 5, 10),
    real_height_cm_lag = runif(n, 0, 10),
    petp_cm = rnorm(n, sd = 5)
  )

  res <- res$ideal_diff_flow_cm

  expect_type(res, "double")
  expect_length(res, n)
})

test_that("compute_ideal_diff_flow_cm(): correct result for petp > 0", {
  compute_ideal_diff_flow_cm(
    ideal_height_cm = c(10, 10),
    real_height_cm_lag = c(8, 8),
    petp_cm = c(1, 3)
    ) |>
    (\(x) x$ideal_diff_flow_cm)() |>
    expect_equal(c(1, -1))

})

test_that("compute_ideal_diff_flow_cm(): correct result for petp < 0", {
  compute_ideal_diff_flow_cm(
    ideal_height_cm = 10,
    real_height_cm_lag = 8,
    petp_cm = -9
  ) |>
    (\(x) x$ideal_diff_flow_cm)() |>
    expect_equal(10)

})



test_that("compute_ideal_flows_cm() returns a (properly) named list", {
  res <- compute_ideal_flows_cm(
    ideal_diff_flow_cm = 1,
    irrigation = TRUE,
    draining = TRUE,
    ideal_flow_rate_cm = 5
  )

  expect_type(res, "list")
  expect_setequal(names(res), c("ideal_inflow_cm", "ideal_outflow_cm"))
})

test_that("compute_ideal_flows_cm() returns the correct structure", {
  set.seed(840)
  n <- rpois(1, 1e3)

  res <- compute_ideal_flows_cm(
    ideal_diff_flow_cm = rnorm(n, sd = 10),
    irrigation = sample(c(TRUE, FALSE), n, replace = TRUE),
    draining = sample(c(TRUE, FALSE), n, replace = TRUE),
    ideal_flow_rate_cm = 5
  )

  res_inflow <- res$ideal_inflow_cm
  res_outflow <- res$ideal_outflow_cm

  expect_type(res_inflow, "double")
  expect_type(res_outflow, "double")
  expect_length(res_inflow, n)
  expect_length(res_outflow, n)
})

test_that("compute_ideal_flows_cm(): inflow - outflow = ideal diff flow", {
  set.seed(840)
  n <- rpois(1, 1e3)
  ideal_diff_flow_cm <- rnorm(n, sd = 10)

  res <- compute_ideal_flows_cm(
    ideal_diff_flow_cm = ideal_diff_flow_cm,
    irrigation = sample(c(TRUE, FALSE), n, replace = TRUE),
    draining = sample(c(TRUE, FALSE), n, replace = TRUE),
    ideal_flow_rate_cm = 5
  )

  expect_equal(res$ideal_inflow_cm - res$ideal_outflow_cm, ideal_diff_flow_cm)
})

test_that("compute_ideal_flows_cm(): correct results when in flux", {
  set.seed(840)
  n <- rpois(1, 1e3)
  ideal_flow_rate_cm <- 5
  ideal_diff_flow_cm <- rnorm(n, sd = 10)

  res <- compute_ideal_flows_cm(
    ideal_diff_flow_cm = ideal_diff_flow_cm,
    irrigation = rep(TRUE, n),
    draining = rep(TRUE, n),
    ideal_flow_rate_cm = ideal_flow_rate_cm
  )

  expect_equal(res$ideal_inflow_cm,
               pmax(ideal_flow_rate_cm, ideal_diff_flow_cm)
               )
})

test_that("compute_ideal_flows_cm(): flows are always positive", {
  set.seed(840)
  n <- rpois(1, 1e3)

  res <- compute_ideal_flows_cm(
    ideal_diff_flow_cm = rnorm(n, sd = 10),
    irrigation = rep(TRUE, n),
    draining = rep(TRUE, n),
    ideal_flow_rate_cm = 5
  )

  expect_gte(min(res$ideal_inflow_cm), 0)
  expect_gte(min(res$ideal_outflow_cm), 0)

})



test_that("compute_real_outflow_m3_s() returns a (properly) named list", {
  res <- compute_real_outflow_m3_s(
    ideal_outflow_cm = c(1, 2, 3),
    area_m2 = c(1, 1, 2),
    capacity_m3_s = 1
  )

  expect_type(res, "list")
  expect_identical(names(res), "real_outflow_m3_s")
})

test_that("compute_real_outflow_m3_s() returns the correct structure", {
  set.seed(840)
  n <- rpois(1, 1e3)

  res <- compute_real_outflow_m3_s(
    ideal_outflow_cm = runif(n, 0, 10),
    area_m2 = runif(n, 1e6, 1e7),
    capacity_m3_s = 5
  )

  expect_type(res$real_outflow_m3_s, "double")
  expect_length(res$real_outflow_m3_s, n)
})

test_that("compute_real_outflow_m3_s(): outflows are always positive", {
  set.seed(840)
  n <- rpois(1, 1e3)
  capacity_m3_s <- 5

  res <- compute_real_outflow_m3_s(
    ideal_outflow_cm = runif(n, 0, 10),
    area_m2 = runif(n, 1e6, 1e7),
    capacity_m3_s = capacity_m3_s
  )

  expect_gte(min(res$real_outflow_m3_s), 0)
})

test_that("compute_real_outflow_m3_s(): sum of real outflows = capacity", {
  set.seed(840)
  n <- rpois(1, 1e3)
  capacity_m3_s <- 5

  res <- compute_real_outflow_m3_s(
    ideal_outflow_cm = runif(n, 0, 10),
    area_m2 = runif(n, 1e6, 1e7),
    capacity_m3_s = capacity_m3_s
  )

  expect_equal(sum(res$real_outflow_m3_s), capacity_m3_s)
})



test_that("compute_real_outflow_cm(): simple check on a concrete case", {
  res <- compute_real_outflow_cm(
    real_outflow_m3_s = 1,
    area_m2 = 1
  )

  expect_equal(res$real_outflow_cm, s_per_day() * 100)
})



test_that("compute_real_inflow_cm(): output is >0 if ideal diff flow is", {
  set.seed(840)
  n <- rpois(1, 1e3)
  ideal_diff_flow_cm <- rnorm(n, sd = 10)


  res <- compute_real_inflow_cm(
    real_outflow_cm = runif(n, 0, 10) * (runif(n) > .5) ,
    ideal_diff_flow_cm = ideal_diff_flow_cm
  )

  expect_gt(
    min(res$real_inflow_cm[ideal_diff_flow_cm > 0]),
    0)
})

test_that("compute_real_inflow_cm(): real-diff-flow = ideal-diff-flow cases", {
  set.seed(840)
  n <- rpois(1, 1e3)
  ideal_diff_flow_cm <- rnorm(n, sd = 10)
  real_outflow_cm <- runif(n, 0, 10) * (runif(n) > .5)

  res <- compute_real_inflow_cm(
    real_outflow_cm = real_outflow_cm,
    ideal_diff_flow_cm = ideal_diff_flow_cm
  )

  real_diff_flow_cm <- res$real_inflow_cm - real_outflow_cm

  # 'cond' captures the fact that either (i) the net flow is expected to be
  # positive, or (ii) there's enough capacity to outflow the expected net flow.
  cond <- real_outflow_cm > -ideal_diff_flow_cm

  expect_equal(real_diff_flow_cm[cond], ideal_diff_flow_cm[cond])
})



test_that("compute_real_inflow_m3_s(): simple check on a concrete case", {
  res <- compute_real_inflow_m3_s(
    real_inflow_cm = 1,
    area_m2 = 1
  )

  expect_equal(res$real_inflow_m3_s, 1 / s_per_day() / 100)
})



test_that("compute_real_height_cm() returns a (properly) named list", {
  res <- compute_real_height_cm(
    real_height_cm_lag = 0,
    petp_cm = 0,
    real_inflow_cm = 1,
    real_outflow_cm = 0
  )

  expect_type(res, "list")
  expect_identical(names(res), "real_height_cm")
})

test_that("compute_real_outflow_m3_s() returns the correct structure", {
  set.seed(840)
  n <- rpois(1, 1e3)

  res <- compute_real_height_cm(
    real_height_cm_lag = runif(n, 0, 20),
    petp_cm = -rnorm(n, sd = 0.5),
    real_inflow_cm = runif(n, 0, 10),
    real_outflow_cm = runif(n,  0, 10)
  )

  expect_type(res$real_height_cm, "double")
  expect_length(res$real_height_cm, n)
})

test_that("compute_real_height_cm(): heights are always positive", {
  skip("compute_real_height_cm(): heights<0 is possible for certain inputs")
  set.seed(840)
  n <- rpois(1, 1e3)

  res <- compute_real_height_cm(
    real_height_cm_lag = runif(n, 0, 20),
    petp_cm = -rnorm(n, sd = 0.5),
    real_inflow_cm = runif(n, 0, 10),
    real_outflow_cm = runif(n,  0, 10)
  )

  expect_gte(min(res$real_height_cm), 0)
})

test_that("compute_real_height_cm(): correct results in simple cases", {
  set.seed(840)

  res <- compute_real_height_cm(
    real_height_cm_lag = c(10, 5, 0, 0),
    petp_cm = c(1, -6, 3, -1),
    real_inflow_cm = c(5, 5, 0, 2),
    real_outflow_cm = c(5, 5, 0, 0)
  )

  expect_equal(res$real_height_cm, c(11, 0, 3, 2))
})

