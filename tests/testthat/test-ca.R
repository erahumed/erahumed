test_that("ca() execution succeeds with valid input", {
  set.seed(840)
  hbl <- albufera_hb_local(date_min = "2010-01-01", date_max = "2010-01-10")
  expect_no_error(ca(hbl))
})

test_that("ca() total applied amounts are equal to expected", {
  set.seed(840)

  hbl <- albufera_hb_local(date_min = "2010-01-01", date_max = "2011-12-31")  # TODO enlarge this date range

  yearly_amounts_clusters <- ca(hbl) |>
    dplyr::mutate(year = format(date, "%Y")) |>
    dplyr::rename(rice_variety = variety) |>
    dplyr::group_by(cluster_id, rice_variety, year) |>
    dplyr::summarise(Acetamiprid = sum(Acetamiprid),
                     Benta = sum(Benta),
                     MCPA = sum(MCPA),
                     Penoxulam = sum(Penoxulam),
                     Cyhalo = sum(Cyhalo),
                     Cicloxidim = sum(Cicloxidim),
                     Azoxy = sum(Azoxy),
                     Difeno = sum(Difeno)
                     )

  yearly_amounts_expected <- albufera_ca_schedules |>
    dplyr::group_by(rice_variety, chemical) |>
    dplyr::summarise(amount = sum(amount)) |>
    tidyr::pivot_wider(id_cols = rice_variety,
                       names_from = chemical,
                       values_from = amount,
                       values_fill = 0)


  res <- yearly_amounts_clusters |> dplyr::anti_join(yearly_amounts_expected)

  expect_equal(nrow(res), 0)
})
