test_that("hba_residence_time throws no errors on regular call", {
  volume <- c(1, 0.9, 0.86, 0.93, 1.1)
  inflow <- c(0.1, 0.12, 0.2, 0.5, 0.2)

  expect_no_error(hba_residence_time(volume, inflow))
})

test_that("hba_residence_time 'k' argument controls level of smoothing", {
  volume <- c(1, 0.9, 0.86, 0.93, 1.1)
  inflow <- c(0.1, 0.12, 0.2, 0.5, 0.2)
  k <- 3

  expect_identical(
    hba_residence_time(volume, inflow, k = k),
    moving_average(volume, k = k) / moving_average(inflow, k = k) / s_per_day()
  )
})



test_that("hba_volume_change() succeeds with valid input", {
  expect_no_error(hba_volume_change(rep(1, 10)))
})

test_that("hba_volume_change() returns a double of the correct length", {
  len <- 10
  res <- hba_volume_change(rep(1, len))
  expect_vector(res, ptype = double(), size = len)
})

test_that("hba_volume_change(): last entry of output == fill_last", {
  fill <- 840
  res <- hba_volume_change(rep(1, 10), fill_last = fill)
  expect_equal(res[length(res)], fill)
})

test_that("hba_volume_change(): volume + differences == lagged volume", {
  volume <- c(4, 2, 6, 4, 6, 3, 7, 8, 4, 9, 1)
  volume_change <- hba_volume_change(volume)
  s <- volume + volume_change
  s <- s[-length(s)]
  expect_equal(s, volume[-1])
})



test_that("hba_flow_balance(): succeeds with valid inputs", {
  expect_no_error(
    hba_flow_balance(outflows = list(a = 1:10, b = 2:11),
                     volume_change = rep(1, 10),
                     volume_change_petp = rep(0.5, 10)
                     )
    )
})

test_that("hba_flow_balance(): returns a dataframe of the correct length", {
  len <- 10
  res <- hba_flow_balance(outflows = list(a = 1:len, b = (1:len) + 5),
                          volume_change = rep(1, len),
                          volume_change_petp = rep(0.5, len)
                          )
  expect_s3_class(res, class = "data.frame")
  expect_equal(nrow(res), len)
})

test_that("hba_flow_balance(): returns df has the required columns", {
  res <- hba_flow_balance(outflows = list(a = 1:10, b = 2:11),
                          volume_change = rep(1, 10),
                          volume_change_petp = rep(0.5, 10)
                          )

  cols <- colnames(res)
  expected_cols <- c(
    "outflow_a", "outflow_b", "outflow_total", "outflow_extra", "inflow_total"
    )
  expect_setequal(cols, expected_cols)
})

test_that("hba_flow_balance(): net balance checks", {
  set.seed(840)
  len <- 1e3
  tol <- 1e-10

  volume_change <- rnorm(len)
  volume_change_petp <- rnorm(len)

  res <- hba_flow_balance(outflows = list(a = runif(len, 0, 1),
                                          b = runif(len, 0, 1)),
                          volume_change = volume_change,
                          volume_change_petp = volume_change_petp
                          )

  zero_check <- res |>
    dplyr::mutate(
      net_flow = (inflow_total - outflow_total) * s_per_day(),
      flow_vol_change = volume_change - volume_change_petp,
    ) |>
    dplyr::filter(
      abs(net_flow - flow_vol_change) > tol * median(abs(net_flow))
      )

  expect_equal(nrow(zero_check), 0)
})

test_that("hba_flow_balance(): sum of outflows equals total", {
  set.seed(840)
  len <- 1e3
  tol <- 1e-10

  volume_change <- rnorm(len)
  volume_change_petp <- rnorm(len)

  res <- hba_flow_balance(outflows = list(a = runif(len, 0, 1),
                                          b = runif(len, 0, 1)),
                          volume_change = volume_change,
                          volume_change_petp = volume_change_petp
                          )

  zero_check <- res |>
    dplyr::mutate(outflow_total_bis = outflow_a + outflow_b + outflow_extra) |>
    dplyr::filter(
      abs(outflow_total - outflow_total_bis) > tol * median(abs(outflow_total))
    )

  expect_equal(nrow(zero_check), 0)
})

test_that("hba_flow_balance(): outflow_extra > 0 requires zero inflow", {
  set.seed(840)
  len <- 1e3
  tol <- 1e-10

  volume_change <- rnorm(len)
  volume_change_petp <- rnorm(len)

  res <- hba_flow_balance(outflows = list(a = runif(len, 0, 1),
                                          b = runif(len, 0, 1)),
                          volume_change = volume_change,
                          volume_change_petp = volume_change_petp
  )

  zero_check <- res |>
    dplyr::filter(
      outflow_extra > tol * mean(abs(outflow_total)),
      inflow_total > tol * mean(abs(outflow_total))
    )

  expect_equal(nrow(zero_check), 0)
})
