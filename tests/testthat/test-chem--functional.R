test_that("Simulation succeeds with chemicals with no degradation",
{
  chem <- acetamiprid()

  chem$kf_day <- 0
  chem$kw_day <- 0
  chem$ks_sat_day <- 0
  chem$ks_unsat_day <- 0

  ms <- new_management_system() |>
    schedule_application(chem, seed_day = 10, emptying_days = 1, type = "ground", amount_kg_ha = 1)

  cm <- new_cluster_map(default_management_system = ms)

  expect_no_error(erahumed_simulation(cluster_map = cm))
})
