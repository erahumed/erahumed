test_that("risk_from_sdss produces the expected output in toy case", {
  # NB: the exponential of the SSD mean parameter is in micrograms per liter
  acetamiprid_at_0.5_acute_risk <- acetamiprid()$ssd_acute_mu / 1e6

  ct_output <- data.frame(
    date = "2000-01-01",
    element_id = "lake",
    chemical_id = 1,
    cw_kg_m3 = c(acetamiprid_at_0.5_acute_risk)
  )
  chemical_db <- get_etc(test_sim_small(), "chemical_db")

  res <- risk_from_ssds(ct_output, chemical_db = chemical_db)

  expect_equal(nrow(res), 2)

  res_chem <- res |>
    (\(.) .[.$stressor_type == "chemical", ])()

  expect_equal(res_chem$paf_acute, 0.5)
})

test_that("Chemicals from same TMoA group combine additively", {
  # Cyhalofop-butyl and Cycloxydim share their TMoA, so half HU of each should
  # sum combine to 1 HU for the TMoA, yielding a risk of 50%

  chemical_db <- get_etc(test_sim_small(), "chemical_db")

  chemical_id <- numeric()
  for (i in seq_along(chemical_db))
    if (chemical_db[[i]]$display_name %in% c("Cyhalofop-butyl", "Cycloxydim"))
      chemical_id <- c(chemical_id, i)

  cyhalo_half_hu <- 0.5 * cyhalofop_butyl()$ssd_acute_mu / 1e6
  cycloxydim_half_hu <- 0.5 * cycloxydim()$ssd_acute_mu / 1e6

  ct_output <- data.frame(
    date = "2000-01-01",
    element_id = "lake",
    chemical_id = chemical_id,
    cw_kg_m3 = c(cyhalo_half_hu, cycloxydim_half_hu)
  )

  res <- risk_from_ssds(ct_output, chemical_db)

  expect_equal(nrow(res), 3)

  res_tmoa <- res |>
    (\(.) .[.$stressor_type == "tmoa", ])()

  expect_equal(res_tmoa$paf_acute, 0.5)
})
