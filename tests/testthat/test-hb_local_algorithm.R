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
