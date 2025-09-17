test_that("chem_input_default() succeeds", {
  expect_no_error(chem_input_defaults())
})

test_that("chem_input_default() returns a list", {
  expect_vector(chem_input_defaults(), ptype = list())
})

input_funs <- list(
  display_name     = chem_input_display_name,
  tmoa_id          = chem_input_tmoa_id,
  MW               = chem_input_MW,
  sol_ppm          = chem_input_sol_ppm,
  koc_cm3_g        = chem_input_koc_cm3_g,
  fet_cm           = chem_input_fet_cm,
  kf_day           = chem_input_kf_day,
  kw_day           = chem_input_kw_day,
  ks_sat_day       = chem_input_ks_sat_day,
  ks_unsat_day     = chem_input_ks_unsat_day,
  kw_temp          = chem_input_kw_temp,
  ks_sat_temp      = chem_input_ks_sat_temp,
  ks_unsat_temp    = chem_input_ks_unsat_temp,
  Q10_kw           = chem_input_Q10_kw,
  Q10_ks_sat       = chem_input_Q10_ks_sat,
  Q10_ks_unsat     = chem_input_Q10_ks_unsat,
  ssd_acute_mu     = chem_input_ssd_acute_mu,
  ssd_acute_sigma  = chem_input_ssd_acute_sigma,
  ssd_chronic_mu   = chem_input_ssd_chronic_mu,
  ssd_chronic_sigma= chem_input_ssd_chronic_sigma
)

test_that("All chem inputs succeed", {
  for (fun in input_funs) {
    expect_no_error( fun("id") )
  }
})



test_that("All chem inputs return a shiny.tag", {
  for (fun in input_funs) {
    obj <- fun("id")
    cl <- class(shiny::tag("", NULL))
    expect_s3_class(obj, cl)
  }
})

