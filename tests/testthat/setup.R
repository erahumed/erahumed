.test_sim_small <- NULL
.test_sim_large <- NULL

test_sim_small <- function() {
  if (is.null(.test_sim_small))
    .test_sim_small <<- test_simulation("2010-01-01", "2010-01-10")

  .test_sim_small
}

test_sim_large <- function() {
  if (is.null(.test_sim_large))
    .test_sim_large <<- test_simulation("2010-01-01", "2011-12-31")

  .test_sim_large
}

test_simulation <- function(date_min, date_max, seed = 840) {
  outflows_df = albufera_outflows |>
    dplyr::filter(date_min <= date, date <= date_max)

  erahumed_simulation() |>
    setup_inp(outflows_df = outflows_df) |>
    run_simulation()
}
