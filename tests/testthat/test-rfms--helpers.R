test_that("get_applications_df0() succeeds", {
  chemical_db <- get_etc(test_sim_small(), "chemical_db")
  system <- bomba()

  expect_no_error(get_applications_df0(system, chemical_db))
})
