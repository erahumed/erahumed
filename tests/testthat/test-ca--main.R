{
  set.seed(840)

  height_thresh_cm <- hbp(test_mod_large())$params$height_thresh_cm

  test_df <- ca(test_mod_large())$output

}

test_that("Execution succeeds with valid input", {
  expect_no_error( compute_ca(test_mod_small()) )
})

test_that("Total number of applications is equal to expected", {
  yearly_amounts_clusters <- test_df |>
    dplyr::mutate(year = format(date, "%Y")) |>
    dplyr::rename(rice_variety = variety) |>
    dplyr::group_by(cluster_id, rice_variety, year) |>
    dplyr::summarise(Acetamiprid = sum(Acetamiprid > 0),
                     Benta = sum(Benta > 0),
                     MCPA = sum(MCPA > 0),
                     Penoxulam = sum(Penoxulam > 0),
                     Cyhalo = sum(Cyhalo > 0),
                     Cicloxidim = sum(Cicloxidim > 0),
                     Azoxy = sum(Azoxy > 0),
                     Difeno = sum(Difeno > 0),
                     .groups = "drop"
                     )

  yearly_amounts_expected <- albufera_ca_schedules |>
    dplyr::group_by(rice_variety, chemical) |>
    dplyr::summarise(applications = sum(kg_per_ha > 0), .groups = "drop") |>
    tidyr::pivot_wider(id_cols = rice_variety,
                       names_from = chemical,
                       values_from = applications,
                       values_fill = 0)


  res <- yearly_amounts_clusters |>
    dplyr::anti_join(yearly_amounts_expected,
                     by = dplyr::join_by(rice_variety,
                                         Acetamiprid,
                                         Benta,
                                         MCPA,
                                         Penoxulam,
                                         Cyhalo,
                                         Cicloxidim,
                                         Azoxy,
                                         Difeno)
                     )

  expect_equal(nrow(res), 0)
})

test_that("Application days have the correct features", {
  chems <- erahumed::albufera_ca_schedules |>
    dplyr::select(chemical, application_type) |>
    dplyr::filter(chemical %in% colnames(test_df)) |>
    dplyr::distinct()

  applications_df <- test_df |>
    dplyr::select(
      real_height_cm, real_irrigation, real_draining,
      dplyr::any_of(chems$chemical)
      ) |>
    tidyr::pivot_longer(
      -c(real_height_cm, real_irrigation, real_draining),
      names_to = "chemical",
      values_to = "amount"
      ) |>
    dplyr::filter(amount > 1e-6) |>
    dplyr::inner_join(chems, by = "chemical")

  test_ground_states <- applications_df |>
    dplyr::filter(application_type == "ground" &
                    (real_irrigation | real_draining))

  test_ground_levels <- applications_df |>
    dplyr::filter(application_type == "ground" &
                    real_height_cm > height_thresh_cm + 1e-6)

  test_aerial_states <- applications_df |>
    dplyr::filter(application_type == "aerial" &
                    !(real_irrigation & real_draining))



  expect_equal(nrow(test_ground_states), 0)
  expect_equal(nrow(test_ground_levels), 0)
  expect_equal(nrow(test_aerial_states), 0)
})


test_that("Simple snapshot is constant", {
  expect_snapshot(digest::digest(test_df))
})
