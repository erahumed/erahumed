test_that("risk_from_sdss produces the expected output in toy case", {
  # NB: the exponential of the SSD mean parameter is in micrograms per liter
  acetamiprid_at_0.5_acute_risk <-
    exp(info_chemicals()$Acetamiprid$ssd_acute_mu) / 1e6

  ct_output <- data.frame(
    date = "2000-01-01",
    element_id = "lake",
    chemical = "Acetamiprid",
    cw_kg_m3 = c(acetamiprid_at_0.5_acute_risk)
  )

  res <- risk_from_ssds(ct_output)

  expect_equal(nrow(res), 2)

  res_chem <- res |>
    (\(.) .[.$stressor_type == "chemical", ])()

  expect_equal(res_chem$paf_acute, 0.5)
})
