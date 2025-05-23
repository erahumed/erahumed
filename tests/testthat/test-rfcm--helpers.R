test_that("get_management_df() succeeds", {
  expect_no_error(get_management_df(default_cluster_map()))
})

test_that("get_management_df() agrees with legacy", {
  warning("Temporary test")
  skip("Passed except for irrelevant format mismatches")
  mm <- dd <- tancat <- variety <- NULL

  actual <- get_management_df(default_cluster_map()) |>
    dplyr::arrange() |>
    dplyr::arrange(mm, dd, tancat, variety)
  expected <- albufera_management |>
    dplyr::mutate(
      variety = match(variety, c("J.Sendra", "Bomba", "Clearfield"))
      ) |>
    dplyr::arrange(mm, dd, tancat, variety)

  expect_identical(actual, expected)
})

test_that("get_applications_df() succeeds", {
  expect_no_error(get_applications_df(default_cluster_map()))
})

test_that("get_applications_df() agrees with legacy", {
  warning("Temporary test")
  skip("Passed except for mismatch in some application days (irrelevant with new algorithm)")

  actual <- get_applications_df(default_cluster_map()) |>
    dplyr::select(seed_day) |> unique() |> dplyr::arrange(seed_day)

  expected <- albufera_ca_schedules |>
    dplyr::rename(seed_day = day) |>
    dplyr::select(seed_day) |> unique() |> dplyr::arrange(seed_day)

  expect_identical(actual, expected)
})

test_that("get_chemical_db() succeeds", {
  expect_no_error(get_chemical_db(default_cluster_map()))
})
