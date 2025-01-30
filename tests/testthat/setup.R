.test_sim_small <- NULL
.test_sim_large <- NULL

test_sim_small <- function(seed = 840, force = FALSE) {
  if (force || is.null(.test_sim_small))
    .test_sim_small <<- test_simulation("2010-01-01", "2010-01-10", seed = seed)

  .test_sim_small
}

test_sim_large <- function(seed = 840, force = FALSE) {
  if (force || is.null(.test_sim_large))
    .test_sim_large <<- test_simulation("2010-01-01", "2011-12-31", seed = seed)

  .test_sim_large
}

test_simulation <- function(date_min, date_max, seed) {
  outflows_df = albufera_outflows |>
    dplyr::filter(date_min <= date, date <= date_max)

  erahumed_simulation() |>
    setup_hydrology(outflows_df = outflows_df, seed = seed) |>
    run_simulation()
}

stopifnot(succeeds(test_sim_small()))
