test_that("hbl_residence_time throws no errors on regular call", {
  volume_m3 <- c(1, 0.9, 0.86, 0.93, 1.1)
  inflow_m3 <- c(0.1, 0.12, 0.2, 0.5, 0.2)

  expect_no_error(hbl_residence_time(volume_m3, inflow_m3))
})

test_that("hbl_residence_time 'k' argument controls level of smoothing", {
  volume_m3 <- c(1, 0.9, 0.86, 0.93, 1.1)
  inflow_m3 <- c(0.1, 0.12, 0.2, 0.5, 0.2)
  k <- 3

  expect_identical(
    hbl_residence_time(volume_m3, inflow_m3, k = k),
    moving_average(volume_m3, k = k) / moving_average(inflow_m3, k = k) / s_per_day()
  )
})



test_that("hbl_diff() succeeds with valid input", {
  expect_no_error(hbl_diff(rep(1, 10)))
})

test_that("hbl_diff() returns a double of the correct length", {
  len <- 10
  res <- hbl_diff(rep(1, len))
  expect_vector(res, ptype = double(), size = len)
})

test_that("hbl_diff(): last entry of output == fill_last", {
  fill <- 840
  res <- hbl_diff(rep(1, 10), fill_last = fill)
  expect_equal(res[length(res)], fill)
})

test_that("hbl_diff(): volume_m3 + differences == lagged volume_m3", {
  volume_m3 <- c(4, 2, 6, 4, 6, 3, 7, 8, 4, 9, 1)
  volume_change_m3 <- hbl_diff(volume_m3)
  s <- volume_m3 + volume_change_m3
  s <- s[-length(s)]
  expect_equal(s, volume_m3[-1])
})



test_that("hbl_flow_balance(): succeeds with valid inputs", {
  expect_no_error(
    hbl_flow_balance(outflows_m3_s = list(a = 1:10, b = 2:11),
                     volume_change_m3 = rep(1, 10),
                     volume_change_petp_m3 = rep(0.5, 10)
                     )
    )
})

test_that("hbl_flow_balance(): returns a dataframe of the correct length", {
  len <- 10
  res <- hbl_flow_balance(outflows_m3_s = list(a = 1:len, b = (1:len) + 5),
                          volume_change_m3 = rep(1, len),
                          volume_change_petp_m3 = rep(0.5, len)
                          )
  expect_s3_class(res, class = "data.frame")
  expect_equal(nrow(res), len)
})

test_that("hbl_flow_balance(): returns df has the required columns", {
  res <- hbl_flow_balance(outflows_m3_s = list(a = 1:10, b = 2:11),
                          volume_change_m3 = rep(1, 10),
                          volume_change_petp_m3 = rep(0.5, 10)
                          )

  cols <- colnames(res)
  expected_cols <- c(
    "outflow_m3_s_a", "outflow_m3_s_b", "outflow_total_m3", "outflow_recirculation_m3_s", "inflow_total_m3"
    )
  expect_setequal(cols, expected_cols)
})

test_that("hbl_flow_balance(): net balance checks", {
  set.seed(840)
  len <- 1e3
  tol <- 1e-10

  volume_change_m3 <- rnorm(len)
  volume_change_petp_m3 <- rnorm(len)

  res <- hbl_flow_balance(outflows_m3_s = list(a = runif(len, 0, 1),
                                          b = runif(len, 0, 1)),
                          volume_change_m3 = volume_change_m3,
                          volume_change_petp_m3 = volume_change_petp_m3
                          )

  zero_check <- res |>
    dplyr::mutate(
      net_flow = (inflow_total_m3 - outflow_total_m3),
      flow_vol_change = volume_change_m3 - volume_change_petp_m3,
    ) |>
    dplyr::filter(
      abs(net_flow - flow_vol_change) > tol * median(abs(net_flow))
      )

  expect_equal(nrow(zero_check), 0)
})

test_that("hbl_flow_balance(): sum of outflows equals total", {
  set.seed(840)
  len <- 1e3
  tol <- 1e-10

  volume_change_m3 <- rnorm(len)
  volume_change_petp_m3 <- rnorm(len)

  res <- hbl_flow_balance(outflows_m3_s = list(a = runif(len, 0, 1),
                                          b = runif(len, 0, 1)),
                          volume_change_m3 = volume_change_m3,
                          volume_change_petp_m3 = volume_change_petp_m3
                          )

  zero_check <- res |>
    dplyr::mutate(outflow_total_bis =
                    s_per_day() * (outflow_m3_s_a + outflow_m3_s_b + outflow_recirculation_m3_s)
                  ) |>
    dplyr::filter(
      abs(outflow_total_m3 - outflow_total_bis) > tol * median(abs(outflow_total_m3))
    )

  expect_equal(nrow(zero_check), 0)
})

test_that("hbl_flow_balance(): outflow_recirculation > 0 requires zero inflow_m3", {
  set.seed(840)
  len <- 1e3

  volume_change_m3 <- rnorm(len)
  volume_change_petp_m3 <- rnorm(len)

  res <- hbl_flow_balance(outflows_m3_s = list(a = runif(len, 0, 1),
                                          b = runif(len, 0, 1)),
                          volume_change_m3 = volume_change_m3,
                          volume_change_petp_m3 = volume_change_petp_m3
  )

  zero_check <- res |>
    dplyr::mutate(
      tol_m3 = 1e-10 * mean(abs(outflow_total_m3)),
      tol_m3_s = tol_m3 / s_per_day()
    ) |>
    dplyr::filter(outflow_recirculation_m3_s > tol_m3,
                  inflow_total_m3 > tol_m3_s)

  expect_equal(nrow(zero_check), 0)
})
