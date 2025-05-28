test_that("PAF columns are probabilities", {
  e <- 1e-10
  paf_chronic <- paf_acute <- NULL

  test_df <- get_output(test_sim_large(), "rc") |>
    dplyr::filter(
      paf_chronic < -e | paf_acute < -e | paf_chronic > 1+e | paf_acute > 1+e
      )

  expect_equal(nrow(test_df), 0)
})

test_that("PAFs are increasing functions of water concentrations", {

  e <- 1e-10
  stressor <- stressor_type <- element_id <- date <- chemical <- paf_acute <- cw_kg_m3 <- NULL

  rc_output <- erahumed:::get_output(test_sim_large(), "rc") |>
    dplyr::filter(stressor_type == "chemical") |>
    dplyr::mutate(chemical = stressor) |>
    dplyr::arrange(element_id, date, chemical)
  ctc_output <- erahumed:::get_output(test_sim_large(), "ctc") |>
    dplyr::arrange(element_id, date, chemical)

  if (nrow(rc_output) != nrow(ctc_output)) {
    "Invalid data preparation in test nrow(rc_output) != nrow(ctc_output)" |>
      warning()
  }

  # Cheap 'merge' exploiting the fact that rows of the two DFs are 1-1
  merged_df <- dplyr::bind_cols(ctc_output[, c("chemical_id", "cw_kg_m3")],
                                rc_output[, c("paf_acute", "paf_chronic")]
                                ) |>
    dplyr::filter(cw_kg_m3 > e)

  test_df <- merged_df |>
    dplyr::group_by(chemical_id) |>
    dplyr::arrange(cw_kg_m3) |>
    dplyr::filter(paf_acute < lag(paf_acute))

  expect_equal(nrow(test_df), 0)
})
