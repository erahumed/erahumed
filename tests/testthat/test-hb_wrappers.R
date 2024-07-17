test_that("hydro_balance_local_snapshot", {
  set.seed(840)
  res <- albufera_hydro_balance_local(date_min = "2010-01-01",
                                      date_max = "2010-01-10"
                                      )
  hash <- digest::sha1(res)
  expect_snapshot(hash)
})
