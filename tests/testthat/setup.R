.test_sim_small <- NULL
.test_sim_large <- NULL

test_sim_small <- function(seed = 840, force = FALSE) {
  if (force || is.null(.test_sim_small))
    .test_sim_small <<- erahumed_simulation(date_start = "2010-01-01",
                                            date_end = "2010-01-10",
                                            seed = seed,
                                            .progress = \(.) {})

  .test_sim_small
}

test_sim_large <- function(seed = 840, force = FALSE) {
  if (force || is.null(.test_sim_large))
    .test_sim_large <<- erahumed_simulation(date_start = "2010-01-01",
                                            date_end = "2011-12-31",
                                            seed = seed,
                                            .progress = \(.) {})

  .test_sim_large
}

test_sim_small()
