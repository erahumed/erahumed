test_that("ca() execution succeeds with valid input", {
  set.seed(840)
  hbl <- albufera_hb_local(date_min = "2010-01-01", date_max = "2010-01-10")
  expect_no_error(ca(hbl))
})

test_that("ca() total number of applications is equal to expected", {
  set.seed(840)

  hbl <- albufera_hb_local(date_min = "2010-01-01", date_max = "2011-12-31")  # TODO enlarge this date range

  yearly_amounts_clusters <- ca(hbl) |>
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

test_that("Simple snapshot is constant", {
  skip("Snapshot test only used for debugging purposes")

  set.seed(840)

  hbl <- albufera_hb_local(date_min = "2010-01-01", date_max = "2011-12-31")

  hash <- digest::digest(ca(hbl))

  expect_snapshot(hash)
})
