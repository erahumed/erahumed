test_that("Returned dataset has the expected number of rows", {
  test_df <- component_output(test_mod_large(), "ct")
  n_clusters <- nrow(albufera_clusters)
  n_days <- length( seq.Date(from = min(test_df$date),
                             to = max(test_df$date),
                             by = "day")
  )

  expect_equal(nrow(test_df), n_clusters * n_days)
})

test_that("simple snapshot is constant", {
  skip_on_ci()  # Gives inconsistent result across different platforms

  test_df <- component_output(test_mod_large(), "ct")
  hash <- digest::digest(test_df)

  expect_snapshot(hash)
})
